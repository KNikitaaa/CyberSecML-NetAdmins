if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "already_mapped",
    "confidence",
    "description",
    "generic_penalty",
    "is_relevant",
    "is_subtechnique",
    "is_tool",
    "llm_status",
    "mapped_confidence",
    "matched_terms",
    "matched_term_count",
    "overlap_weight",
    "phrase_bonus",
    "platforms",
    "record_id",
    "retrieval_rank",
    "retrieval_score",
    "retrieval_text",
    "retrieval_tokens",
    "specificity_bonus",
    "specificity_score",
    "tactic_bonus",
    "tactic_name",
    "tactic_names",
    "tactic_shortname",
    "tactic_shortnames",
    "technique_id",
    "technique_name",
    "technique_weight"
  ))
}

.rag_refinement_stopwords <- function() {
  c(
    "about", "after", "again", "against", "allow", "allows", "along", "also", "among", "analyze",
    "attack", "attacker", "attackers", "attacks", "between", "build", "capability", "capabilities",
    "common", "could", "enable", "example", "framework", "from", "into", "like", "more", "most",
    "other", "over", "platform", "platforms", "provide", "security", "such", "than", "that", "their",
    "there", "these", "they", "this", "tool", "tools", "toward", "used", "uses", "using", "utility",
    "with", "within", "without", "your"
  )
}

.rag_low_signal_tokens <- function() {
  c(
    "access", "active", "application", "applications", "cloud", "code", "command", "commands",
    "control", "data", "domain", "domains", "dns", "execution", "exploit", "exploits",
    "file", "files", "host", "hosts", "network", "networks", "payload", "powershell",
    "process", "processes", "python", "script", "scripts", "server", "servers", "service",
    "services", "software", "system", "systems", "task", "tasks", "traffic", "user", "users",
    "utility", "utilities", "vulnerability", "vulnerabilities", "web"
  )
}

.rag_generic_technique_ids <- function() {
  c("T1059.006", "T1053.002", "T1583.004", "T1584.004", "T1587.004", "T1588.005", "T1592.002")
}

.rag_specificity_score <- function(tokens) {
  tokens <- unique(as.character(tokens %||% character()))

  if (length(tokens) == 0) {
    return(0)
  }

  low_signal <- .rag_low_signal_tokens()
  informative <- tokens[!(tokens %in% low_signal)]
  weighted_terms <- informative[nchar(informative) >= 5L | grepl("^(t|ta)[0-9]+([.][0-9]+)?$", informative)]
  base_score <- length(unique(weighted_terms))

  if (base_score == 0L && length(informative) > 0L) {
    base_score <- 1L
  }

  as.numeric(base_score)
}

.rag_timestamp <- function(value = Sys.time()) {
  format(value, "%Y-%m-%d %H:%M:%S")
}

.rag_resolve_mitre_attack_path <- function(path = NULL) {
  candidates <- unique(c(
    path,
    file.path(getwd(), "data", "mitre_attack.rda"),
    file.path(getwd(), "..", "data", "mitre_attack.rda"),
    file.path(getwd(), "..", "..", "data", "mitre_attack.rda")
  ))

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("MITRE ATT&CK dataset file was not found. Expected data/mitre_attack.rda.", call. = FALSE)
}

load_mitre_attack_dataset <- function(path = NULL) {
  resolved_path <- .rag_resolve_mitre_attack_path(path)
  environment <- new.env(parent = emptyenv())
  load(resolved_path, envir = environment)

  if (!exists("mitre_attack", envir = environment, inherits = FALSE)) {
    stop(sprintf("MITRE ATT&CK dataset file does not define 'mitre_attack': %s", resolved_path), call. = FALSE)
  }

  environment$mitre_attack
}

.rag_tokenize <- function(text) {
  cleaned <- .clean_candidate_text(text, max_chars = 20000L)

  if (is.na(cleaned) || !nzchar(cleaned)) {
    return(character(0))
  }

  tokens <- tolower(cleaned)
  tokens <- gsub("[^a-z0-9.]+", " ", tokens)
  tokens <- stringr::str_squish(tokens)
  tokens <- unlist(strsplit(tokens, " ", fixed = TRUE), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]

  keep <- grepl("^(t|ta)[0-9]+([.][0-9]+)?$", tokens) |
    (nchar(tokens) >= 3 & !(tokens %in% .rag_refinement_stopwords()))

  unique(tokens[keep])
}

.rag_compose_technique_text <- function(technique_id, technique_name, description, tactic_names, tactic_shortnames, platforms) {
  parts <- c(
    technique_id,
    technique_name,
    description,
    paste(tactic_names, collapse = " "),
    paste(tactic_shortnames, collapse = " "),
    paste(platforms, collapse = " ")
  )

  parts <- parts[!is.na(parts) & nzchar(parts)]
  paste(parts, collapse = "\n\n")
}

.rag_idf <- function(token, token_document_frequency, document_count) {
  document_frequency <- token_document_frequency[[token]]

  if (is.null(document_frequency) || is.na(document_frequency)) {
    return(log(document_count + 1) + 1)
  }

  log((document_count + 1) / (document_frequency + 1)) + 1
}

.rag_tool_query_text <- function(record) {
  parts <- c(
    .normalize_string(record$assessed_name),
    .normalize_string(record$name),
    .normalize_string(record$entity_type),
    .normalize_string(record$category_ru),
    .normalize_string(record$raw_description),
    .normalize_string(record$raw_text),
    .normalize_string(record$llm_input_text),
    paste(.normalize_value_or(record$target_platforms[[1]], character(0)), collapse = " ")
  )

  .clean_candidate_text(paste(parts[!is.na(parts) & nzchar(parts)], collapse = "\n\n"), max_chars = 5500L)
}

#' Build and cache a retrieval index for MITRE techniques
#'
#' @param mitre_attack Optional MITRE matrix data frame.
#' @param mitre_attack_path Optional path to `data/mitre_attack.rda`.
#' @param data_dir Directory where cached artifacts are stored.
#' @param output_path Output path for the cached retrieval index.
#' @param overwrite Whether to rebuild the index when a cached copy exists.
#'
#' @return Named list with indexed MITRE techniques and token statistics.
build_mitre_refinement_index <- function(
  mitre_attack = NULL,
  mitre_attack_path = NULL,
  data_dir = get_default_data_dir(),
  output_path = file.path(data_dir, "mitre_refinement_index.rds"),
  overwrite = FALSE
) {
  if (!isTRUE(overwrite) && file.exists(output_path)) {
    return(readRDS(output_path))
  }

  mitre_matrix <- mitre_attack
  if (is.null(mitre_matrix)) {
    mitre_matrix <- load_mitre_attack_dataset(mitre_attack_path)
  }

  if (!is.data.frame(mitre_matrix) || nrow(mitre_matrix) == 0) {
    stop("mitre_attack must be a non-empty data frame.", call. = FALSE)
  }

  group_keys <- paste(
    mitre_matrix[["technique_id"]],
    mitre_matrix[["technique_name"]],
    mitre_matrix[["description"]],
    mitre_matrix[["is_subtechnique"]],
    sep = "\r"
  )
  group_rows <- split(seq_len(nrow(mitre_matrix)), group_keys)
  grouped <- tibble::tibble(
    technique_id = vapply(group_rows, function(indexes) mitre_matrix[["technique_id"]][[indexes[[1]]]], character(1)),
    technique_name = vapply(group_rows, function(indexes) mitre_matrix[["technique_name"]][[indexes[[1]]]], character(1)),
    description = vapply(group_rows, function(indexes) mitre_matrix[["description"]][[indexes[[1]]]], character(1)),
    is_subtechnique = vapply(group_rows, function(indexes) as.logical(mitre_matrix[["is_subtechnique"]][[indexes[[1]]]]), logical(1)),
    tactic_names = lapply(group_rows, function(indexes) sort(unique(stats::na.omit(mitre_matrix[["tactic_name"]][indexes])))),
    tactic_shortnames = lapply(group_rows, function(indexes) sort(unique(stats::na.omit(mitre_matrix[["tactic_shortname"]][indexes])))),
    platforms = lapply(group_rows, function(indexes) {
      values <- unlist(mitre_matrix[["platforms"]][indexes], use.names = FALSE)
      sort(unique(stats::na.omit(values)))
    })
  )
  grouped[["retrieval_text"]] <- vapply(
    seq_len(nrow(grouped)),
    function(index) {
      .rag_compose_technique_text(
        technique_id = grouped[["technique_id"]][[index]],
        technique_name = grouped[["technique_name"]][[index]],
        description = grouped[["description"]][[index]],
        tactic_names = grouped[["tactic_names"]][[index]],
        tactic_shortnames = grouped[["tactic_shortnames"]][[index]],
        platforms = grouped[["platforms"]][[index]]
      )
    },
    character(1)
  )
  grouped[["retrieval_tokens"]] <- lapply(grouped[["retrieval_text"]], .rag_tokenize)

  token_document_frequency <- grouped$retrieval_tokens |>
    lapply(unique) |>
    unlist(use.names = FALSE) |>
    table() |>
    as.list()

  index <- list(
    built_at = .rag_timestamp(Sys.time()),
    document_count = nrow(grouped),
    token_document_frequency = token_document_frequency,
    techniques = grouped
  )

  save_pipeline_rds(index, output_path)
  log_message(sprintf("Saved MITRE refinement index with %s techniques to %s", nrow(grouped), output_path))
  index
}

#' Retrieve top MITRE techniques for a tool description
#'
#' @param query_text Tool text to retrieve against.
#' @param mitre_index Optional retrieval index previously generated by
#'   `build_mitre_refinement_index()`.
#' @param top_k Maximum number of techniques to return.
#' @param min_score Minimum retrieval score to keep.
#' @param data_dir Directory containing the cached index.
#' @param index_path Cached retrieval index path.
#' @param mitre_attack Optional MITRE matrix used to build the index when absent.
#' @param mitre_attack_path Optional path to `data/mitre_attack.rda`.
#'
#' @return Tibble with ranked MITRE technique candidates.
retrieve_relevant_techniques <- function(
  query_text,
  mitre_index = NULL,
  top_k = 8L,
  min_score = 0.12,
  data_dir = get_default_data_dir(),
  index_path = file.path(data_dir, "mitre_refinement_index.rds"),
  mitre_attack = NULL,
  mitre_attack_path = NULL
) {
  index <- mitre_index
  if (is.null(index)) {
    index <- build_mitre_refinement_index(
      mitre_attack = mitre_attack,
      mitre_attack_path = mitre_attack_path,
      data_dir = data_dir,
      output_path = index_path,
      overwrite = FALSE
    )
  }

  query_tokens <- .rag_tokenize(query_text)
  if (length(query_tokens) == 0) {
    return(tibble::tibble(
      technique_id = character(),
      technique_name = character(),
      tactic_names = list(),
      is_subtechnique = logical(),
      retrieval_score = numeric(),
      retrieval_rank = integer(),
      matched_terms = list()
    ))
  }

  document_count <- index$document_count
  query_weight_sum <- sum(vapply(query_tokens, .rag_idf, numeric(1), token_document_frequency = index$token_document_frequency, document_count = document_count))
  normalized_query <- tolower(.clean_candidate_text(query_text, max_chars = 12000L))

  techniques <- tibble::as_tibble(index$techniques)
  techniques[["matched_terms"]] <- lapply(techniques[["retrieval_tokens"]], function(tokens) intersect(query_tokens, tokens))
  techniques[["matched_term_count"]] <- vapply(techniques[["matched_terms"]], length, integer(1))
  techniques[["specificity_score"]] <- vapply(techniques[["matched_terms"]], .rag_specificity_score, numeric(1))
  techniques[["overlap_weight"]] <- vapply(
    techniques[["matched_terms"]],
    function(tokens) {
      sum(vapply(tokens, .rag_idf, numeric(1), token_document_frequency = index$token_document_frequency, document_count = document_count))
    },
    numeric(1)
  )
  techniques[["technique_weight"]] <- vapply(
    techniques[["retrieval_tokens"]],
    function(tokens) {
      sum(vapply(tokens, .rag_idf, numeric(1), token_document_frequency = index$token_document_frequency, document_count = document_count))
    },
    numeric(1)
  )
  techniques[["phrase_bonus"]] <- vapply(
    seq_len(nrow(techniques)),
    function(index) {
      if (stringr::str_detect(normalized_query, stringr::fixed(tolower(techniques[["technique_id"]][[index]])))) {
        return(0.80)
      }
      if (stringr::str_detect(normalized_query, stringr::fixed(tolower(techniques[["technique_name"]][[index]])))) {
        return(0.35)
      }
      0
    },
    numeric(1)
  )
  techniques[["tactic_bonus"]] <- vapply(
    techniques[["tactic_names"]],
    function(values) {
      if (any(vapply(values, function(value) stringr::str_detect(normalized_query, stringr::fixed(tolower(value))), logical(1)))) {
        return(0.08)
      }
      0
    },
    numeric(1)
  )
  techniques[["specificity_bonus"]] <- pmin(0.18, techniques[["specificity_score"]] * 0.04)
  techniques[["generic_penalty"]] <- vapply(
    seq_len(nrow(techniques)),
    function(index) {
      technique_id <- techniques[["technique_id"]][[index]]
      technique_name <- tolower(techniques[["technique_name"]][[index]] %||% "")
      matched_terms <- techniques[["matched_terms"]][[index]]
      matched_count <- techniques[["matched_term_count"]][[index]]
      specificity_score <- techniques[["specificity_score"]][[index]]
      exact_phrase <- techniques[["phrase_bonus"]][[index]] >= 0.35
      low_signal_terms <- matched_terms[matched_terms %in% .rag_low_signal_tokens()]
      generic_name <- stringr::str_detect(technique_name, "\\b(server|service|services|software|python|powershell|dns|vulnerability|vulnerabilities|payload|command)\\b")

      penalty <- 0

      if (!exact_phrase && technique_id %in% .rag_generic_technique_ids()) {
        penalty <- penalty + 0.18
      }

      if (!exact_phrase && generic_name) {
        penalty <- penalty + 0.12
      }

      if (!exact_phrase && matched_count > 0L && length(low_signal_terms) == matched_count) {
        penalty <- penalty + 0.12
      }

      if (!exact_phrase && matched_count <= 1L) {
        penalty <- penalty + 0.08
      }

      if (!exact_phrase && specificity_score <= 1) {
        penalty <- penalty + 0.08
      }

      min(penalty, 0.32)
    },
    numeric(1)
  )
  techniques[["retrieval_score"]] <- vapply(
    seq_len(nrow(techniques)),
    function(index) {
      overlap_weight <- techniques[["overlap_weight"]][[index]]
      technique_weight <- techniques[["technique_weight"]][[index]]
      phrase_bonus <- techniques[["phrase_bonus"]][[index]]
      tactic_bonus <- techniques[["tactic_bonus"]][[index]]
      specificity_bonus <- techniques[["specificity_bonus"]][[index]]
      generic_penalty <- techniques[["generic_penalty"]][[index]]

      if (overlap_weight <= 0 || query_weight_sum <= 0 || technique_weight <= 0) {
        return(0)
      }

      max(0, (overlap_weight / sqrt(query_weight_sum * technique_weight)) + phrase_bonus + tactic_bonus + specificity_bonus - generic_penalty)
    },
    numeric(1)
  )
  scored <- techniques[techniques[["retrieval_score"]] >= min_score, , drop = FALSE]
  if (nrow(scored) > 0) {
    scored <- scored[order(-scored[["retrieval_score"]], scored[["technique_id"]], scored[["technique_name"]]), , drop = FALSE]
    scored <- utils::head(scored, as.integer(top_k))
    scored[["retrieval_rank"]] <- seq_len(nrow(scored))
    scored <- tibble::as_tibble(scored[c("technique_id", "technique_name", "tactic_names", "is_subtechnique", "retrieval_score", "retrieval_rank", "matched_terms")])
  } else {
    scored <- tibble::tibble(
      technique_id = character(),
      technique_name = character(),
      tactic_names = list(),
      is_subtechnique = logical(),
      retrieval_score = numeric(),
      retrieval_rank = integer(),
      matched_terms = list()
    )
  }

  scored
}

#' Run retrieval-based MITRE refinement suggestions for assessed tools
#'
#' @param assessment_results Optional unified assessment results.
#' @param mitre_mappings Optional current MITRE mappings.
#' @param mitre_attack Optional MITRE matrix.
#' @param data_dir Directory containing pipeline artifacts.
#' @param index_path Path for the cached retrieval index.
#' @param output_path Output path for refinement candidates.
#' @param top_k Maximum number of candidates per tool.
#' @param min_score Minimum retrieval score to keep.
#' @param overwrite_index Whether to rebuild the index.
#' @param mitre_attack_path Optional path to `data/mitre_attack.rda`.
#'
#' @return Tibble with per-tool MITRE refinement candidates.
run_mitre_refinement <- function(
  assessment_results = NULL,
  mitre_mappings = NULL,
  mitre_attack = NULL,
  data_dir = get_default_data_dir(),
  index_path = file.path(data_dir, "mitre_refinement_index.rds"),
  output_path = file.path(data_dir, "mitre_refinement_candidates.rds"),
  top_k = 8L,
  min_score = 0.18,
  overwrite_index = FALSE,
  mitre_attack_path = NULL
) {
  assessments <- assessment_results
  mappings <- mitre_mappings

  if (is.null(assessments)) {
    assessments <- load_pipeline_table("llm_assessments", rds_path = file.path(data_dir, "tool_assessment_results.rds"), required = TRUE)
  }

  if (is.null(mappings)) {
    mappings <- load_pipeline_table("mitre_mappings", rds_path = file.path(data_dir, "tool_mitre_mappings.rds"), required = FALSE)
  }

  relevant_mask <- assessments[["llm_status"]] == "success"
  relevant_mask[is.na(relevant_mask)] <- FALSE
  relevant_mask <- relevant_mask & (assessments[["is_relevant"]] %in% TRUE)
  relevant_mask <- relevant_mask & (assessments[["is_tool"]] %in% TRUE)
  relevant_tools <- tibble::as_tibble(assessments[relevant_mask, , drop = FALSE])

  if (nrow(relevant_tools) == 0) {
    empty <- tibble::tibble(
      record_id = character(),
      assessed_name = character(),
      technique_id = character(),
      technique_name = character(),
      tactic_names = list(),
      is_subtechnique = logical(),
      retrieval_score = numeric(),
      retrieval_rank = integer(),
      matched_terms = list(),
      already_mapped = logical(),
      mapped_confidence = numeric()
    )
    save_pipeline_rds(empty, output_path)
    return(empty)
  }

  index <- build_mitre_refinement_index(
    mitre_attack = mitre_attack,
    mitre_attack_path = mitre_attack_path,
    data_dir = data_dir,
    output_path = index_path,
    overwrite = overwrite_index
  )

  candidate_rows <- lapply(seq_len(nrow(relevant_tools)), function(index_row) {
    record <- relevant_tools[index_row, , drop = FALSE]
    query_text <- .rag_tool_query_text(record)
    candidates <- retrieve_relevant_techniques(
      query_text = query_text,
      mitre_index = index,
      top_k = top_k,
      min_score = min_score,
      data_dir = data_dir,
      index_path = index_path
    )

    if (nrow(candidates) == 0) {
      return(candidates |>
        dplyr::mutate(
          record_id = character(),
          assessed_name = character(),
          already_mapped = logical(),
          mapped_confidence = numeric(),
          .before = 1L
        ))
    }

    existing_rows <- if (is.data.frame(mappings) && nrow(mappings) > 0) {
      relevant_mappings <- mappings[mappings[["record_id"]] == record[["record_id"]][[1]], , drop = FALSE]
      if (nrow(relevant_mappings) > 0) {
        mapping_groups <- split(seq_len(nrow(relevant_mappings)), relevant_mappings[["technique_id"]])
        tibble::tibble(
          technique_id = names(mapping_groups),
          mapped_confidence = vapply(
            mapping_groups,
            function(indexes) {
              values <- relevant_mappings[["confidence"]][indexes]
              if (all(is.na(values))) {
                return(NA_real_)
              }
              max(values, na.rm = TRUE)
            },
            numeric(1)
          )
        )
      } else {
        tibble::tibble(technique_id = character(), mapped_confidence = numeric())
      }
    } else {
      tibble::tibble(technique_id = character(), mapped_confidence = numeric())
    }

    mapped_index <- match(candidates[["technique_id"]], existing_rows[["technique_id"]])
    candidates[["mapped_confidence"]] <- existing_rows[["mapped_confidence"]][mapped_index]
    candidates[["record_id"]] <- record[["record_id"]][[1]]
    candidates[["assessed_name"]] <- record[["assessed_name"]][[1]]
    candidates[["already_mapped"]] <- !is.na(candidates[["mapped_confidence"]])
    candidates <- candidates[c(
      "record_id",
      "assessed_name",
      "technique_id",
      "technique_name",
      "tactic_names",
      "is_subtechnique",
      "retrieval_score",
      "retrieval_rank",
      "matched_terms",
      "already_mapped",
      "mapped_confidence"
    )]
    tibble::as_tibble(candidates)
  })

  refinement_candidates <- tibble::as_tibble(dplyr::bind_rows(candidate_rows))
  if (nrow(refinement_candidates) > 0) {
    refinement_candidates <- refinement_candidates[
      order(
        refinement_candidates[["record_id"]],
        refinement_candidates[["retrieval_rank"]],
        -refinement_candidates[["retrieval_score"]],
        refinement_candidates[["technique_id"]]
      ),
      ,
      drop = FALSE
    ]
  }

  save_pipeline_rds(refinement_candidates, output_path)
  log_message(sprintf("Saved %s MITRE refinement candidate rows to %s", nrow(refinement_candidates), output_path))
  refinement_candidates
}