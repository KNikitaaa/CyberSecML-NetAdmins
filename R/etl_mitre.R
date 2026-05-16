#' Fetch MITRE ATT&CK STIX data
#'
#' Downloads the MITRE ATT&CK STIX bundle for a given domain.
#'
#' @param domain ATT&CK domain slug. Defaults to `"enterprise-attack"`.
#' @param ref Git reference in the MITRE ATT&CK STIX repository.
#' @param url Optional direct URL or local path to a STIX JSON file.
#'
#' @return Parsed STIX bundle as a named list.
fetch_mitre_attack_data <- function(
  domain = "enterprise-attack",
  ref = "master",
  url = NULL
) {
  source_url <- url

  if (is.null(source_url) || !nzchar(source_url)) {
    source_url <- sprintf(
      "https://raw.githubusercontent.com/mitre-attack/attack-stix-data/%s/%s/%s.json",
      ref,
      domain,
      domain
    )
  }

  safe_run(
    expr = function() {
      jsonlite::fromJSON(source_url, simplifyDataFrame = FALSE)
    },
    error_context = sprintf("Failed to load MITRE ATT&CK STIX data from %s", source_url)
  )
}

.mitre_extract_objects <- function(stix_data) {
  objects <- stix_data$objects

  if (is.null(objects) || !is.list(objects)) {
    stop("MITRE STIX bundle does not contain a valid 'objects' list.", call. = FALSE)
  }

  objects
}

.mitre_scalar <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || (is.character(first_value) && !nzchar(first_value))) {
    return(default)
  }

  as.character(first_value)
}

.mitre_flag <- function(value) {
  isTRUE(value) || identical(value, 1L) || identical(value, 1)
}

.mitre_character_vector <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(character(0))
  }

  unique(as.character(unlist(value, use.names = FALSE)))
}

.mitre_external_id <- function(external_references) {
  if (is.null(external_references) || length(external_references) == 0) {
    return(NA_character_)
  }

  for (reference in external_references) {
    source_name <- .mitre_scalar(reference$source_name, default = "")
    external_id <- .mitre_scalar(reference$external_id, default = "")

    if (identical(source_name, "mitre-attack") && nzchar(external_id)) {
      return(external_id)
    }
  }

  NA_character_
}

.mitre_phase_names <- function(kill_chain_phases) {
  if (is.null(kill_chain_phases) || length(kill_chain_phases) == 0) {
    return(character(0))
  }

  phase_names <- vapply(
    kill_chain_phases,
    FUN.VALUE = character(1),
    FUN = function(phase) {
      .mitre_scalar(phase$phase_name, default = NA_character_)
    }
  )

  unique(stats::na.omit(phase_names))
}

.mitre_is_active <- function(object) {
  !.mitre_flag(object$revoked) && !.mitre_flag(object$x_mitre_deprecated)
}

#' Parse MITRE ATT&CK tactics
#'
#' @param stix_data Parsed STIX bundle.
#'
#' @return Tibble with normalized MITRE tactics.
parse_mitre_tactics <- function(stix_data) {
  objects <- .mitre_extract_objects(stix_data)

  tactics <- Filter(
    f = function(object) {
      identical(.mitre_scalar(object$type, default = ""), "x-mitre-tactic") &&
        .mitre_is_active(object)
    },
    x = objects
  )

  rows <- lapply(
    tactics,
    function(tactic) {
      tibble::tibble(
        stix_id = .mitre_scalar(tactic$id),
        tactic_id = .mitre_external_id(tactic$external_references),
        tactic_name = .mitre_scalar(tactic$name),
        tactic_shortname = .mitre_scalar(tactic$x_mitre_shortname),
        description = .mitre_scalar(tactic$description),
        created = .mitre_scalar(tactic$created),
        modified = .mitre_scalar(tactic$modified)
      )
    }
  )

  dplyr::bind_rows(rows)
}

#' Parse MITRE ATT&CK techniques and sub-techniques
#'
#' @param stix_data Parsed STIX bundle.
#'
#' @return Tibble with normalized techniques in long format by tactic.
parse_mitre_techniques <- function(stix_data) {
  objects <- .mitre_extract_objects(stix_data)

  techniques <- Filter(
    f = function(object) {
      identical(.mitre_scalar(object$type, default = ""), "attack-pattern") &&
        .mitre_is_active(object)
    },
    x = objects
  )

  rows <- lapply(
    techniques,
    function(technique) {
      tactic_shortnames <- .mitre_phase_names(technique$kill_chain_phases)

      if (length(tactic_shortnames) == 0) {
        tactic_shortnames <- NA_character_
      }

      tibble::tibble(
        stix_id = rep(.mitre_scalar(technique$id), length(tactic_shortnames)),
        technique_id = rep(.mitre_external_id(technique$external_references), length(tactic_shortnames)),
        technique_name = rep(.mitre_scalar(technique$name), length(tactic_shortnames)),
        tactic_shortname = tactic_shortnames,
        description = rep(.mitre_scalar(technique$description), length(tactic_shortnames)),
        is_subtechnique = rep(.mitre_flag(technique$x_mitre_is_subtechnique), length(tactic_shortnames)),
        platforms = rep(
          paste(.mitre_character_vector(technique$x_mitre_platforms), collapse = "; "),
          length(tactic_shortnames)
        ),
        created = rep(.mitre_scalar(technique$created), length(tactic_shortnames)),
        modified = rep(.mitre_scalar(technique$modified), length(tactic_shortnames))
      )
    }
  )

  dplyr::bind_rows(rows)
}

#' Build a MITRE ATT&CK matrix table
#'
#' @param stix_data Parsed STIX bundle.
#'
#' @return Tibble joining techniques to tactics.
get_mitre_matrix <- function(stix_data) {
  tactics <- parse_mitre_tactics(stix_data)
  techniques <- parse_mitre_techniques(stix_data)

  tactic_lookup <- tactics[, c(
    "tactic_id",
    "tactic_name",
    "tactic_shortname",
    "description",
    "created",
    "modified"
  ), drop = FALSE]
  names(tactic_lookup)[names(tactic_lookup) == "description"] <- "tactic_description"
  names(tactic_lookup)[names(tactic_lookup) == "created"] <- "tactic_created"
  names(tactic_lookup)[names(tactic_lookup) == "modified"] <- "tactic_modified"

  result <- dplyr::left_join(techniques, tactic_lookup, by = "tactic_shortname")
  order_index <- order(
    result[["tactic_name"]],
    result[["technique_id"]],
    result[["technique_name"]],
    na.last = TRUE
  )

  result[order_index, , drop = FALSE]
}

#' Save a prepared MITRE ATT&CK dataset
#'
#' @param stix_data Optional parsed STIX bundle. When `NULL`, data is fetched.
#' @param output_path File path for the `.rda` artifact.
#' @param domain ATT&CK domain slug.
#' @param ref Git reference in the MITRE ATT&CK STIX repository.
#'
#' @return Invisibly returns the generated matrix tibble.
save_mitre_attack_dataset <- function(
  stix_data = NULL,
  output_path = file.path("data", "mitre_attack.rda"),
  domain = "enterprise-attack",
  ref = "master"
) {
  mitre_stix <- stix_data

  if (is.null(mitre_stix)) {
    mitre_stix <- fetch_mitre_attack_data(domain = domain, ref = ref)
  }

  mitre_attack <- get_mitre_matrix(mitre_stix)
  ensure_dir(dirname(output_path))
  save(mitre_attack, file = output_path, compress = "bzip2")
  invisible(mitre_attack)
}
