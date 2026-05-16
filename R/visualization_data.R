.visualization_slug <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  normalized <- tolower(as.character(value[[1]]))
  normalized <- gsub("[^a-z0-9]+", "_", normalized)
  normalized <- gsub("(^_+|_+$)", "", normalized)
  normalized <- gsub("_+", "_", normalized)

  if (!nzchar(normalized)) {
    return(NA_character_)
  }

  normalized
}

.visualization_confidence_score <- function(value) {
  numeric_value <- suppressWarnings(as.numeric(value[[1]]))

  if (is.na(numeric_value)) {
    return(NA_real_)
  }

  if (numeric_value > 1) {
    numeric_value <- numeric_value / 100
  }

  max(min(numeric_value, 1), 0)
}

.visualization_entity_priority <- function(entity_type) {
  normalized_type <- .visualization_slug(entity_type)

  switch(
    normalized_type,
    framework = 1.00,
    utility_suite = 0.95,
    utility = 0.90,
    exploit = 0.88,
    script_collection = 0.70,
    library = 0.35,
    non_tool = 0.10,
    news_or_advisory = 0.05,
    0.50
  )
}

.visualization_detail_score <- function(short_description_ru, long_description_ru, capabilities_ru) {
  short_length <- nchar(dplyr::coalesce(short_description_ru, ""), type = "chars")
  long_length <- nchar(dplyr::coalesce(long_description_ru, ""), type = "chars")
  capability_count <- length(.llm_character_vector(capabilities_ru))

  short_score <- min(short_length / 220, 1)
  long_score <- min(long_length / 1200, 1)
  capability_score <- min(capability_count / 8, 1)

  round((short_score * 0.25) + (long_score * 0.50) + (capability_score * 0.25), 4)
}

.visualization_mitre_score <- function(mitre_tactics, mitre_technique_ids) {
  tactic_count <- length(.normalize_value_or(mitre_tactics, character(0)))
  technique_count <- length(.normalize_value_or(mitre_technique_ids, character(0)))

  tactic_score <- min(tactic_count / 5, 1)
  technique_score <- min(technique_count / 8, 1)

  round((tactic_score * 0.4) + (technique_score * 0.6), 4)
}

.visualization_post_llm_score <- function(pre_llm_score, confidence_score, entity_priority_score, detail_score, mitre_score) {
  heuristic_score <- suppressWarnings(as.numeric(pre_llm_score[[1]]))
  if (is.na(heuristic_score)) {
    heuristic_score <- 0
  }

  heuristic_score <- min(max(heuristic_score / 100, 0), 1)

  round(
    (heuristic_score * 0.30) +
      (confidence_score * 0.25) +
      (detail_score * 0.20) +
      (mitre_score * 0.15) +
      (entity_priority_score * 0.10),
    4
  )
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "active_in_ui",
    "assessed_name",
    "confidence_score",
    "entity_type",
    "existing_first_ui_added_at",
    "first_ui_added_at",
    "last_ui_seen_at",
    "llm_status",
    "mitre_technique_count",
    "record_id",
    "visualization_rank"
  ))
}

.visualization_long_description <- function(summary_ru, purpose_ru, capabilities_ru, reason_ru) {
  capability_text <- .llm_character_vector(capabilities_ru)
  capability_block <- if (length(capability_text) > 0) {
    paste("Ключевые возможности:", paste(capability_text, collapse = "; "))
  } else {
    NA_character_
  }

  parts <- c(summary_ru, purpose_ru, capability_block, reason_ru)
  parts <- parts[!is.na(parts) & nzchar(parts)]

  if (length(parts) == 0) {
    return(NA_character_)
  }

  paste(parts, collapse = "\n\n")
}

.visualization_build_filter_tags <- function(source, entity_type, category_ru, mitre_technique_ids, mitre_tactics) {
  tags <- c(
    if (!is.na(source) && nzchar(source)) paste0("source:", .visualization_slug(source)),
    if (!is.na(entity_type) && nzchar(entity_type)) paste0("entity:", .visualization_slug(entity_type)),
    if (!is.na(category_ru) && nzchar(category_ru)) paste0("category:", .visualization_slug(category_ru)),
    if (length(mitre_technique_ids) > 0) paste0("mitre:technique:", mitre_technique_ids),
    if (length(mitre_tactics) > 0) paste0("mitre:tactic:", vapply(mitre_tactics, .visualization_slug, character(1)))
  )

  unique(tags[!is.na(tags) & nzchar(tags)])
}

.visualization_empty_tools <- function() {
  tibble::tibble(
    record_id = character(),
    assessed_name = character(),
    source = character(),
    source_type = character(),
    url = character(),
    date_found = character(),
    entity_type = character(),
    category_ru = character(),
    short_description_ru = character(),
    long_description_ru = character(),
    summary_ru = character(),
    purpose_ru = character(),
    capabilities_ru = list(),
    reason_ru = character(),
    pre_llm_score = numeric(),
    pre_llm_priority = character(),
    confidence_score = numeric(),
    detail_score = numeric(),
    mitre_score = numeric(),
    entity_priority_score = numeric(),
    visualization_score = numeric(),
    overall_confidence = numeric(),
    llm_provider = character(),
    llm_model = character(),
    mitre_tactics = list(),
    mitre_technique_ids = list(),
    mitre_technique_names = list(),
    mitre_tactic_count = integer(),
    mitre_technique_count = integer(),
    filter_tags = list(),
    visualization_rank = integer(),
    mitre_matrix = list(),
    first_ui_added_at = character(),
    last_ui_seen_at = character()
  )
}

.visualization_empty_matrix <- function() {
  tibble::tibble(
    record_id = character(),
    assessed_name = character(),
    source = character(),
    url = character(),
    entity_type = character(),
    category_ru = character(),
    technique_id = character(),
    technique_name = character(),
    tactic = character(),
    confidence = numeric(),
    reasoning_ru = character(),
    tactic_tag = character(),
    technique_tag = character()
  )
}

.visualization_empty_history <- function() {
  tibble::tibble(
    record_id = character(),
    assessed_name = character(),
    source = character(),
    entity_type = character(),
    first_ui_added_at = character(),
    last_ui_seen_at = character(),
    active_in_ui = logical(),
    last_visualization_rank = integer(),
    last_confidence_score = numeric(),
    last_mitre_technique_count = integer()
  )
}

.visualization_load_history <- function(path) {
  if (!file.exists(path)) {
    return(.visualization_empty_history())
  }

  value <- readRDS(path)
  if (!is.data.frame(value)) {
    return(.visualization_empty_history())
  }

  value
}

.visualization_update_history <- function(visualization_tools, history_path) {
  history <- .visualization_load_history(history_path)
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  current_rows <- tibble::tibble(
    record_id = as.character(visualization_tools[["record_id"]]),
    assessed_name = as.character(visualization_tools[["assessed_name"]]),
    source = as.character(visualization_tools[["source"]]),
    entity_type = as.character(visualization_tools[["entity_type"]]),
    first_ui_added_at = rep(timestamp, nrow(visualization_tools)),
    last_ui_seen_at = rep(timestamp, nrow(visualization_tools)),
    active_in_ui = rep(TRUE, nrow(visualization_tools)),
    last_visualization_rank = as.integer(visualization_tools[["visualization_rank"]]),
    last_confidence_score = as.numeric(visualization_tools[["confidence_score"]]),
    last_mitre_technique_count = as.integer(visualization_tools[["mitre_technique_count"]])
  )

  if (nrow(history) > 0 && nrow(current_rows) > 0) {
    existing_first_ui_added_at <- history[["first_ui_added_at"]][match(current_rows[["record_id"]], history[["record_id"]])]
    current_rows[["first_ui_added_at"]] <- dplyr::coalesce(existing_first_ui_added_at, current_rows[["first_ui_added_at"]])
  }

  historical_inactive <- history[!(history[["record_id"]] %in% current_rows[["record_id"]]), , drop = FALSE]
  if (nrow(historical_inactive) > 0) {
    historical_inactive[["active_in_ui"]] <- FALSE
  }

  updated_history <- tibble::as_tibble(dplyr::bind_rows(current_rows, historical_inactive))
  if (nrow(updated_history) > 0) {
    history_order <- order(!updated_history[["active_in_ui"]], -xtfrm(updated_history[["last_ui_seen_at"]]), updated_history[["assessed_name"]])
    updated_history <- updated_history[history_order, , drop = FALSE]
  }

  save_pipeline_rds(updated_history, history_path)

  current_rows_with_history <- current_rows[c("record_id", "first_ui_added_at", "last_ui_seen_at")]

  list(
    history = updated_history,
    current_rows = current_rows_with_history
  )
}

#' Build visualization-ready tool and matrix datasets
#'
#' @param assessment_results Optional unified assessment results data frame.
#' @param mitre_mappings Optional MITRE mapping data frame.
#' @param data_dir Directory containing pipeline artifacts.
#' @param output_path Output path for the visualization tool layer.
#' @param matrix_output_path Output path for the visualization MITRE matrix layer.
#' @param duckdb_path DuckDB database path for persisted visualization tables.
#' @param write_duckdb Whether to persist visualization outputs into DuckDB.
#'
#' @return Named list with `visualization_tools` and `visualization_matrix` tibbles.
build_visualization_dataset <- function(
  assessment_results = NULL,
  mitre_mappings = NULL,
  data_dir = get_default_data_dir(),
  output_path = file.path(data_dir, "visualization_tools.rds"),
  matrix_output_path = file.path(data_dir, "visualization_tool_matrix.rds"),
  history_output_path = file.path(data_dir, "visualization_tool_history.rds"),
  duckdb_path = get_default_duckdb_path(data_dir),
  write_duckdb = TRUE
) {
  assessments <- assessment_results
  mappings <- mitre_mappings

  if (is.null(assessments)) {
    assessments <- load_pipeline_table("llm_assessments", rds_path = file.path(data_dir, "tool_assessment_results.rds"), db_path = duckdb_path, required = TRUE)
  }

  if (is.null(mappings)) {
    mappings <- load_pipeline_table("mitre_mappings", rds_path = file.path(data_dir, "tool_mitre_mappings.rds"), db_path = duckdb_path, required = TRUE)
  }

  relevant_mask <- assessments[["llm_status"]] == "success"
  relevant_mask[is.na(relevant_mask)] <- FALSE
  relevant_mask <- relevant_mask & (assessments[["is_relevant"]] %in% TRUE)
  relevant_mask <- relevant_mask & (assessments[["is_tool"]] %in% TRUE)

  documentation_like <- vapply(
    seq_len(nrow(assessments)),
    function(index) {
      .normalize_is_documentation_like_candidate(
        name = assessments[["assessed_name"]][[index]],
        raw_text = paste(
          assessments[["summary_ru"]][[index]],
          assessments[["purpose_ru"]][[index]],
          assessments[["reason_ru"]][[index]],
          collapse = " "
        ),
        metadata = assessments[["metadata"]][[index]],
        url = assessments[["url"]][[index]]
      )
    },
    logical(1)
  )

  relevant_tools <- tibble::as_tibble(assessments[relevant_mask & !documentation_like, , drop = FALSE])
  if (nrow(relevant_tools) > 0) {
    pre_llm_score <- suppressWarnings(as.numeric(relevant_tools[["pre_llm_score"]]))
    overall_confidence <- suppressWarnings(as.numeric(relevant_tools[["overall_confidence"]]))
    pre_llm_score[is.na(pre_llm_score)] <- -Inf
    overall_confidence[is.na(overall_confidence)] <- -Inf
    relevant_order <- order(-pre_llm_score, -overall_confidence, relevant_tools[["assessed_name"]])
    relevant_tools <- relevant_tools[relevant_order, , drop = FALSE]
  }

  if (nrow(relevant_tools) == 0) {
    matrix_rows <- .visualization_empty_matrix()
    visualization_tools <- .visualization_empty_tools()
    history_result <- .visualization_update_history(visualization_tools, history_output_path)

    save_pipeline_rds(visualization_tools, output_path)
    save_pipeline_rds(matrix_rows, matrix_output_path)

    if (isTRUE(write_duckdb)) {
      init_pipeline_duckdb(duckdb_path)
      write_duckdb_table(visualization_tools, "visualization_tools", db_path = duckdb_path, overwrite = TRUE)
      write_duckdb_table(matrix_rows, "visualization_tool_matrix", db_path = duckdb_path, overwrite = TRUE)
      write_duckdb_table(history_result$history, "visualization_tool_history", db_path = duckdb_path, overwrite = TRUE)
    }

    log_message(sprintf("Saved %s visualization tool rows to %s", nrow(visualization_tools), output_path))
    log_message(sprintf("Saved %s visualization matrix rows to %s", nrow(matrix_rows), matrix_output_path))
    log_message(sprintf("Saved %s visualization history rows to %s", nrow(history_result$history), history_output_path))

    return(list(
      visualization_tools = visualization_tools,
      visualization_tool_matrix = matrix_rows,
      visualization_tool_history = history_result$history
    ))
  }

  tool_columns <- c("record_id", "assessed_name", "source", "url", "entity_type", "category_ru")
  tool_rows <- unique(relevant_tools[tool_columns])
  relevant_mapping_rows <- mappings[mappings[["record_id"]] %in% tool_rows[["record_id"]], , drop = FALSE]

  if (nrow(relevant_mapping_rows) == 0) {
    matrix_rows <- .visualization_empty_matrix()
  } else {
    matrix_rows <- merge(
      relevant_mapping_rows,
      tool_rows,
      by = "record_id",
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", "_tool")
    )
    if ("assessed_name_tool" %in% names(matrix_rows)) {
      matrix_rows[["assessed_name"]] <- dplyr::coalesce(matrix_rows[["assessed_name_tool"]], matrix_rows[["assessed_name"]])
    }
    matrix_rows[["tactic_tag"]] <- paste0("mitre:tactic:", vapply(matrix_rows[["tactic"]], .visualization_slug, character(1)))
    matrix_rows[["technique_tag"]] <- paste0("mitre:technique:", matrix_rows[["technique_id"]])
    matrix_rows <- tibble::as_tibble(matrix_rows[c(
      "record_id",
      "assessed_name",
      "source",
      "url",
      "entity_type",
      "category_ru",
      "technique_id",
      "technique_name",
      "tactic",
      "confidence",
      "reasoning_ru",
      "tactic_tag",
      "technique_tag"
    )])
  }

  matrix_summary <- if (nrow(matrix_rows) == 0) {
    tibble::tibble(
      record_id = character(),
      mitre_technique_ids = list(),
      mitre_technique_names = list(),
      mitre_tactics = list(),
      mitre_matrix = list()
    )
  } else {
    row_groups <- split(seq_len(nrow(matrix_rows)), matrix_rows[["record_id"]])
    tibble::tibble(
      record_id = names(row_groups),
      mitre_technique_ids = lapply(row_groups, function(indexes) unique(matrix_rows[["technique_id"]][indexes])),
      mitre_technique_names = lapply(row_groups, function(indexes) unique(matrix_rows[["technique_name"]][indexes])),
      mitre_tactics = lapply(row_groups, function(indexes) unique(matrix_rows[["tactic"]][indexes])),
      mitre_matrix = lapply(
        row_groups,
        function(indexes) {
          tibble::as_tibble(matrix_rows[indexes, c("technique_id", "technique_name", "tactic", "confidence", "reasoning_ru"), drop = FALSE])
        }
      )
    )
  }

  visualization_tools <- tibble::as_tibble(relevant_tools)
  summary_match <- match(visualization_tools[["record_id"]], matrix_summary[["record_id"]])
  empty_character_list <- rep(list(character(0)), nrow(visualization_tools))
  empty_matrix_list <- rep(list(tibble::tibble(
    technique_id = character(),
    technique_name = character(),
    tactic = character(),
    confidence = numeric(),
    reasoning_ru = character()
  )), nrow(visualization_tools))

  visualization_tools[["mitre_technique_ids"]] <- empty_character_list
  visualization_tools[["mitre_technique_names"]] <- empty_character_list
  visualization_tools[["mitre_tactics"]] <- empty_character_list
  visualization_tools[["mitre_matrix"]] <- empty_matrix_list

  matched_rows <- which(!is.na(summary_match))
  if (length(matched_rows) > 0) {
    visualization_tools[["mitre_technique_ids"]][matched_rows] <- matrix_summary[["mitre_technique_ids"]][summary_match[matched_rows]]
    visualization_tools[["mitre_technique_names"]][matched_rows] <- matrix_summary[["mitre_technique_names"]][summary_match[matched_rows]]
    visualization_tools[["mitre_tactics"]][matched_rows] <- matrix_summary[["mitre_tactics"]][summary_match[matched_rows]]
    visualization_tools[["mitre_matrix"]][matched_rows] <- matrix_summary[["mitre_matrix"]][summary_match[matched_rows]]
  }

  visualization_tools[["short_description_ru"]] <- visualization_tools[["summary_ru"]]
  visualization_tools[["long_description_ru"]] <- vapply(
    seq_len(nrow(visualization_tools)),
    function(index) {
      .visualization_long_description(
        visualization_tools[["summary_ru"]][[index]],
        visualization_tools[["purpose_ru"]][[index]],
        visualization_tools[["capabilities_ru"]][[index]],
        visualization_tools[["reason_ru"]][[index]]
      )
    },
    character(1)
  )
  visualization_tools[["confidence_score"]] <- vapply(visualization_tools[["overall_confidence"]], .visualization_confidence_score, numeric(1))
  visualization_tools[["entity_priority_score"]] <- vapply(visualization_tools[["entity_type"]], .visualization_entity_priority, numeric(1))
  visualization_tools[["detail_score"]] <- vapply(
    seq_len(nrow(visualization_tools)),
    function(index) {
      .visualization_detail_score(
        visualization_tools[["summary_ru"]][[index]],
        visualization_tools[["long_description_ru"]][[index]],
        visualization_tools[["capabilities_ru"]][[index]]
      )
    },
    numeric(1)
  )
  visualization_tools[["mitre_score"]] <- vapply(
    seq_len(nrow(visualization_tools)),
    function(index) {
      .visualization_mitre_score(
        mitre_tactics = .normalize_value_or(visualization_tools[["mitre_tactics"]][[index]], character(0)),
        mitre_technique_ids = .normalize_value_or(visualization_tools[["mitre_technique_ids"]][[index]], character(0))
      )
    },
    numeric(1)
  )
  visualization_tools[["filter_tags"]] <- lapply(
    seq_len(nrow(visualization_tools)),
    function(index) {
      .visualization_build_filter_tags(
        source = visualization_tools[["source"]][[index]],
        entity_type = visualization_tools[["entity_type"]][[index]],
        category_ru = visualization_tools[["category_ru"]][[index]],
        mitre_technique_ids = .normalize_value_or(visualization_tools[["mitre_technique_ids"]][[index]], character(0)),
        mitre_tactics = .normalize_value_or(visualization_tools[["mitre_tactics"]][[index]], character(0))
      )
    }
  )
  visualization_tools[["visualization_score"]] <- vapply(
    seq_len(nrow(visualization_tools)),
    function(index) {
      .visualization_post_llm_score(
        visualization_tools[["pre_llm_score"]][[index]],
        visualization_tools[["confidence_score"]][[index]],
        visualization_tools[["entity_priority_score"]][[index]],
        visualization_tools[["detail_score"]][[index]],
        visualization_tools[["mitre_score"]][[index]]
      )
    },
    numeric(1)
  )
  visualization_tools[["mitre_tactic_count"]] <- lengths(lapply(visualization_tools[["mitre_tactics"]], .normalize_value_or, default = character(0)))
  visualization_tools[["mitre_technique_count"]] <- lengths(lapply(visualization_tools[["mitre_technique_ids"]], .normalize_value_or, default = character(0)))

  if (nrow(visualization_tools) > 0) {
    visualization_order <- order(
      -visualization_tools[["visualization_score"]],
      -visualization_tools[["confidence_score"]],
      -visualization_tools[["detail_score"]],
      -suppressWarnings(as.numeric(visualization_tools[["pre_llm_score"]])),
      visualization_tools[["assessed_name"]]
    )
    visualization_tools <- visualization_tools[visualization_order, , drop = FALSE]
  }
  visualization_tools[["visualization_rank"]] <- seq_len(nrow(visualization_tools))
  visualization_tools <- tibble::as_tibble(visualization_tools[c(
    "record_id",
    "assessed_name",
    "source",
    "source_type",
    "url",
    "date_found",
    "entity_type",
    "category_ru",
    "short_description_ru",
    "long_description_ru",
    "summary_ru",
    "purpose_ru",
    "capabilities_ru",
    "reason_ru",
    "pre_llm_score",
    "pre_llm_priority",
    "confidence_score",
    "detail_score",
    "mitre_score",
    "entity_priority_score",
    "visualization_score",
    "overall_confidence",
    "llm_provider",
    "llm_model",
    "mitre_tactics",
    "mitre_technique_ids",
    "mitre_technique_names",
    "mitre_tactic_count",
    "mitre_technique_count",
    "filter_tags",
    "visualization_rank",
    "mitre_matrix"
  )])

  history_result <- .visualization_update_history(visualization_tools, history_output_path)
  visualization_tools <- visualization_tools |>
    dplyr::left_join(history_result$current_rows, by = "record_id")

  save_pipeline_rds(visualization_tools, output_path)
  save_pipeline_rds(matrix_rows, matrix_output_path)

  if (isTRUE(write_duckdb)) {
    init_pipeline_duckdb(duckdb_path)
    write_duckdb_table(visualization_tools, "visualization_tools", db_path = duckdb_path, overwrite = TRUE)
    write_duckdb_table(matrix_rows, "visualization_tool_matrix", db_path = duckdb_path, overwrite = TRUE)
    write_duckdb_table(history_result$history, "visualization_tool_history", db_path = duckdb_path, overwrite = TRUE)
  }

  log_message(sprintf("Saved %s visualization tool rows to %s", nrow(visualization_tools), output_path))
  log_message(sprintf("Saved %s visualization matrix rows to %s", nrow(matrix_rows), matrix_output_path))
  log_message(sprintf("Saved %s visualization history rows to %s", nrow(history_result$history), history_output_path))

  list(
    visualization_tools = visualization_tools,
    visualization_tool_matrix = matrix_rows,
    visualization_tool_history = history_result$history
  )
}