.assessment_empty_results <- function() {
  tibble::tibble(
    record_id = character(),
    name = character(),
    source = character(),
    source_type = character(),
    url = character(),
    raw_description = character(),
    raw_text = character(),
    llm_input_text = character(),
    date_found = character(),
    pre_llm_score = numeric(),
    pre_llm_priority = character(),
    pre_llm_candidate_type = character(),
    pre_llm_should_process = logical(),
    pre_llm_reasons = list(),
    metadata = list(),
    llm_provider = character(),
    llm_base_url = character(),
    llm_model = character(),
    llm_processed_at = character(),
    llm_status = character(),
    llm_error = character(),
    is_relevant = logical(),
    entity_type = character(),
    is_tool = logical(),
    assessed_name = character(),
    summary_ru = character(),
    purpose_ru = character(),
    capabilities_ru = list(),
    target_platforms = list(),
    category_ru = character(),
    overall_confidence = numeric(),
    reason_ru = character()
  )
}

.assessment_empty_mitre_results <- function() {
  tibble::tibble(
    record_id = character(),
    assessed_name = character(),
    technique_id = character(),
    technique_name = character(),
    tactic = character(),
    confidence = numeric(),
    reasoning_ru = character()
  )
}

.assessment_system_prompt <- function() {
  paste(
    "You analyze offensive security candidates in one pass.",
    "Decide whether the candidate is relevant, determine its entity type, write concise Russian descriptions for UI consumption, and map it to MITRE ATT&CK conservatively.",
    "Russian is required for summary_ru, purpose_ru, capabilities_ru, category_ru, and reason_ru.",
    "MITRE technique_name and tactic may remain in English.",
    "Reject plain news, advisories, writeups, training material, generic libraries, unrelated repositories, documentation-first repositories such as cheat sheets, playbooks, exam prep, walkthroughs, and tool references, and Markdown-only repositories that do not appear to contain executable tooling.",
    "Return a single JSON object using these exact top-level keys: is_relevant, entity_type, is_tool, name, summary_ru, purpose_ru, capabilities_ru, target_platforms, category_ru, overall_confidence, reason_ru, mitre_classifications.",
    "Do not rename keys. Do not use aliases such as relevant or mitre_attack.",
    "capabilities_ru and target_platforms must be JSON arrays of strings, even when there is only one item.",
    "Each mitre_classifications item must contain technique_id, technique_name, tactic, confidence, reasoning_ru.",
    "Do not assign a MITRE technique unless the candidate text contains explicit evidence for that behavior.",
    "Avoid generic Reconnaissance and Resource Development mappings, including T1595, unless the tool's primary behavior clearly performs that exact function.",
    "It is better to return an empty mitre_classifications array than to guess. Weak inferred mappings must be omitted.",
    "Return only valid JSON matching the provided schema."
  )
}

build_unified_tool_assessment_prompt <- function(record) {
  if (!is.data.frame(record) || nrow(record) != 1) {
    stop("record must be a one-row data frame.", call. = FALSE)
  }

  metadata <- record$metadata[[1]]
  llm_text <- .clean_candidate_text(record$raw_text, max_chars = 3500L)
  metadata_json <- jsonlite::toJSON(
    .normalize_value_or(metadata, list()),
    auto_unbox = TRUE,
    null = "null"
  )

  sprintf(
    paste(
      "Candidate name: %s",
      "Source: %s",
      "Source type: %s",
      "URL: %s",
      "Heuristic candidate type: %s",
      "Heuristic score: %s",
      "Heuristic reasons: %s",
      "Metadata JSON: %s",
      "Raw text:",
      "%s",
      "",
      "Task:",
      "1. Decide whether the candidate is relevant to offensive security tooling.",
      "2. Determine entity_type: utility, utility_suite, framework, script_collection, exploit, library, news_or_advisory, or non_tool.",
      "3. Produce Russian user-facing fields: summary_ru, purpose_ru, capabilities_ru, category_ru, reason_ru.",
      "4. Repositories whose main value is documentation, study notes, command references, cheat sheets, playbooks, or certification prep are not tools even if they mention real utilities.",
      "5. Markdown-only repositories with notes, helpers, or references but no sign of executable scripts, binaries, agents, or runnable tooling should be treated as non_tool.",
      "6. Keep MITRE mapping conservative. Do not map a technique without explicit evidence in the candidate text. Avoid broad Reconnaissance / Resource Development guesses such as T1595 unless the tool directly performs that behavior. If evidence is weak, return an empty list.",
      "7. For irrelevant candidates still return valid Russian text such as 'Не релевантно' instead of empty strings.",
      sep = "\n"
    ),
    .llm_optional_string(record$name),
    .llm_optional_string(record$source),
    .llm_optional_string(record$source_type),
    .llm_optional_string(record$url),
    .llm_optional_string(record$pre_llm_candidate_type),
    ifelse(is.na(record$pre_llm_score[[1]]), "NA", as.character(record$pre_llm_score[[1]])),
    paste(.normalize_value_or(record$pre_llm_reasons[[1]], character(0)), collapse = "; "),
    metadata_json,
    .llm_optional_string(llm_text)
  )
}

.assessment_success_row <- function(record, parsed, provider, base_url, model) {
  assessment <- parsed$assessment

  tibble::tibble(
    record_id = record$record_id,
    name = record$name,
    source = record$source,
    source_type = record$source_type,
    url = record$url,
    raw_description = record$raw_description,
    raw_text = record$raw_text,
    llm_input_text = .clean_candidate_text(record$raw_text, max_chars = 3500L),
    date_found = record$date_found,
    pre_llm_score = record$pre_llm_score,
    pre_llm_priority = record$pre_llm_priority,
    pre_llm_candidate_type = record$pre_llm_candidate_type,
    pre_llm_should_process = record$pre_llm_should_process,
    pre_llm_reasons = record$pre_llm_reasons,
    metadata = record$metadata,
    llm_provider = provider,
    llm_base_url = base_url,
    llm_model = model,
    llm_processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    llm_status = "success",
    llm_error = NA_character_,
    is_relevant = assessment$is_relevant,
    entity_type = assessment$entity_type,
    is_tool = assessment$is_tool,
    assessed_name = assessment$name,
    summary_ru = assessment$summary_ru,
    purpose_ru = assessment$purpose_ru,
    capabilities_ru = assessment$capabilities_ru,
    target_platforms = assessment$target_platforms,
    category_ru = assessment$category_ru,
    overall_confidence = assessment$overall_confidence,
    reason_ru = assessment$reason_ru
  )
}

.assessment_skipped_row <- function(record, provider, base_url, llm_status, llm_error = NA_character_) {
  tibble::tibble(
    record_id = record$record_id,
    name = record$name,
    source = record$source,
    source_type = record$source_type,
    url = record$url,
    raw_description = record$raw_description,
    raw_text = record$raw_text,
    llm_input_text = .clean_candidate_text(record$raw_text, max_chars = 3500L),
    date_found = record$date_found,
    pre_llm_score = record$pre_llm_score,
    pre_llm_priority = record$pre_llm_priority,
    pre_llm_candidate_type = record$pre_llm_candidate_type,
    pre_llm_should_process = record$pre_llm_should_process,
    pre_llm_reasons = record$pre_llm_reasons,
    metadata = record$metadata,
    llm_provider = provider,
    llm_base_url = base_url,
    llm_model = NA_character_,
    llm_processed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    llm_status = llm_status,
    llm_error = llm_error,
    is_relevant = NA,
    entity_type = NA_character_,
    is_tool = NA,
    assessed_name = NA_character_,
    summary_ru = NA_character_,
    purpose_ru = NA_character_,
    capabilities_ru = list(character(0)),
    target_platforms = list(character(0)),
    category_ru = NA_character_,
    overall_confidence = NA_real_,
    reason_ru = NA_character_
  )
}

.assessment_mitre_rows <- function(record_id, assessed_name, parsed) {
  mappings <- parsed$mitre_classifications

  if (nrow(mappings) == 0) {
    return(.assessment_empty_mitre_results())
  }

  mappings |>
    dplyr::mutate(
      record_id = record_id,
      assessed_name = assessed_name,
      .before = 1L
    )
}

get_assessment_candidates <- function(
  normalized_data = NULL,
  data_dir = get_default_data_dir(),
  max_records = NULL
) {
  get_validation_candidates(
    normalized_data = normalized_data,
    data_dir = data_dir,
    max_records = max_records
  )
}

.assessment_empty_queue <- function() {
  tibble::tibble(
    record_id = character(),
    name = character(),
    source = character(),
    source_type = character(),
    date_found = character(),
    pre_llm_score = numeric(),
    pre_llm_priority = character(),
    pre_llm_candidate_type = character(),
    pre_llm_should_process = logical(),
    queue_status = character(),
    llm_status = character(),
    llm_processed_at = character(),
    llm_error = character(),
    assessed_name = character(),
    entity_type = character(),
    is_relevant = logical(),
    is_tool = logical(),
    overall_confidence = numeric()
  )
}

build_llm_processing_queue <- function(
  normalized_data = NULL,
  assessment_results = NULL,
  data_dir = get_default_data_dir(),
  output_path = file.path(data_dir, "llm_processing_queue.rds"),
  duckdb_path = NULL,
  write_duckdb = FALSE,
  run_id = NA_character_
) {
  if (is.null(duckdb_path)) {
    duckdb_path <- get_default_duckdb_path(data_dir)
  }

  inputs <- normalized_data
  if (is.null(inputs)) {
    inputs <- load_pipeline_table("normalized_candidates", rds_path = file.path(data_dir, "normalized_tools.rds"), db_path = duckdb_path, required = TRUE)
  }

  results <- assessment_results
  if (is.null(results)) {
    results <- load_pipeline_table("llm_assessments", rds_path = file.path(data_dir, "tool_assessment_results.rds"), db_path = duckdb_path, required = FALSE)
  }

  if (!is.data.frame(inputs)) {
    stop("normalized_data must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(results)) {
    results <- .assessment_empty_results()
  }

  assessment_index <- results[!duplicated(results[["record_id"]]), c(
    "record_id",
    "llm_status",
    "llm_processed_at",
    "llm_error",
    "assessed_name",
    "entity_type",
    "is_relevant",
    "is_tool",
    "overall_confidence"
  ), drop = FALSE]

  queue <- dplyr::left_join(inputs, assessment_index, by = "record_id")

  queue_status <- ifelse(
    !(queue[["pre_llm_should_process"]] %in% TRUE),
    "filtered_out_pre_llm",
    ifelse(
      is.na(queue[["llm_status"]]),
      "pending_not_sent",
      ifelse(
        queue[["llm_status"]] == "error",
        "retry_after_error",
        ifelse(
          queue[["llm_status"]] == "success",
          "processed_success",
          ifelse(
            queue[["llm_status"]] == "skipped_pre_filter",
            "filtered_out_pre_llm",
            paste0("status_", queue[["llm_status"]])
          )
        )
      )
    )
  )

  queue[["queue_status"]] <- queue_status

  status_levels <- c(
    "pending_not_sent",
    "retry_after_error",
    "processed_success",
    "filtered_out_pre_llm"
  )
  status_rank <- match(queue[["queue_status"]], status_levels)
  status_rank[is.na(status_rank)] <- length(status_levels) + 1L
  score_values <- queue[["pre_llm_score"]]
  score_values[is.na(score_values)] <- -Inf
  date_values <- queue[["date_found"]]
  date_values[is.na(date_values)] <- ""
  name_values <- queue[["name"]]

  order_index <- order(status_rank, -score_values, -xtfrm(date_values), name_values, na.last = TRUE)
  queue <- queue[order_index, c(
    "record_id",
    "name",
    "source",
    "source_type",
    "date_found",
    "pre_llm_score",
    "pre_llm_priority",
    "pre_llm_candidate_type",
    "pre_llm_should_process",
    "queue_status",
    "llm_status",
    "llm_processed_at",
    "llm_error",
    "assessed_name",
    "entity_type",
    "is_relevant",
    "is_tool",
    "overall_confidence"
  ), drop = FALSE]

  save_pipeline_rds(queue, output_path)

  if (isTRUE(write_duckdb)) {
    write_pipeline_snapshot_table(queue, "llm_processing_queue", db_path = duckdb_path, history_table = "llm_processing_queue_history", run_id = run_id, pipeline_stage = "assessment")
  }

  queue
}

.assessment_load_previous_results <- function(path, duckdb_path = NULL) {
  value <- load_pipeline_table("llm_assessments", rds_path = path, db_path = duckdb_path, required = FALSE)
  if (!is.data.frame(value)) {
    return(.assessment_empty_results())
  }

  value
}

.assessment_load_previous_mitre <- function(path, duckdb_path = NULL) {
  value <- load_pipeline_table("mitre_mappings", rds_path = path, db_path = duckdb_path, required = FALSE)
  if (!is.data.frame(value)) {
    return(.assessment_empty_mitre_results())
  }

  value
}

.assessment_same_text <- function(left, right) {
  left_value <- dplyr::coalesce(as.character(left), "")
  right_value <- dplyr::coalesce(as.character(right), "")
  identical(left_value, right_value)
}

.assessment_find_reusable_successes <- function(inputs, previous_results) {
  previous_success <- previous_results[previous_results[["llm_status"]] == "success", , drop = FALSE]

  if (nrow(inputs) == 0 || nrow(previous_success) == 0) {
    return(character(0))
  }

  current_view <- inputs[, c("record_id", "name", "source", "source_type", "url", "raw_description", "raw_text"), drop = FALSE]
  names(current_view) <- c(
    "record_id",
    "current_name",
    "current_source",
    "current_source_type",
    "current_url",
    "current_raw_description",
    "current_raw_text"
  )

  previous_view <- previous_success[, c("record_id", "name", "source", "source_type", "url", "raw_description", "raw_text"), drop = FALSE]
  names(previous_view) <- c(
    "record_id",
    "previous_name",
    "previous_source",
    "previous_source_type",
    "previous_url",
    "previous_raw_description",
    "previous_raw_text"
  )

  comparison <- dplyr::inner_join(current_view, previous_view, by = "record_id")

  reusable <- comparison[["record_id"]][vapply(
    seq_len(nrow(comparison)),
    function(index) {
      .assessment_same_text(comparison[["current_name"]][[index]], comparison[["previous_name"]][[index]]) &&
        .assessment_same_text(comparison[["current_source"]][[index]], comparison[["previous_source"]][[index]]) &&
        .assessment_same_text(comparison[["current_source_type"]][[index]], comparison[["previous_source_type"]][[index]]) &&
        .assessment_same_text(comparison[["current_url"]][[index]], comparison[["previous_url"]][[index]]) &&
        .assessment_same_text(comparison[["current_raw_description"]][[index]], comparison[["previous_raw_description"]][[index]]) &&
        .assessment_same_text(comparison[["current_raw_text"]][[index]], comparison[["previous_raw_text"]][[index]])
    },
    logical(1)
  )]

  unique(reusable)
}

#' Run the unified LLM assessment stage
#'
#' @param normalized_data Optional normalized tibble.
#' @param data_dir Directory containing normalized artifacts.
#' @param output_path Output path for all assessment results.
#' @param relevant_output_path Output path for relevant tool-only rows.
#' @param mitre_output_path Output path for MITRE mappings extracted from the assessment.
#' @param queue_output_path Output path for the LLM processing queue snapshot.
#' @param duckdb_path DuckDB database path for persisted pipeline tables.
#' @param write_duckdb Whether to persist outputs into DuckDB tables.
#' @param run_id Optional pipeline run identifier for DuckDB history rows.
#' @param provider LLM provider name.
#' @param model LLM model name.
#' @param base_url Optional provider base URL override.
#' @param max_records Optional limit on the number of processed candidates.
#' @param api_key Provider API key.
#'
#' @return Named list with `assessment_results`, `relevant_tools`, and `tool_mitre_mappings` tibbles.
run_unified_tool_assessment <- function(
  normalized_data = NULL,
  data_dir = get_default_data_dir(),
  output_path = file.path(data_dir, "tool_assessment_results.rds"),
  relevant_output_path = file.path(data_dir, "relevant_tools.rds"),
  mitre_output_path = file.path(data_dir, "tool_mitre_mappings.rds"),
  queue_output_path = file.path(data_dir, "llm_processing_queue.rds"),
  duckdb_path = NULL,
  write_duckdb = TRUE,
  run_id = NA_character_,
  provider = get_default_llm_provider(),
  model = get_default_llm_model(provider),
  base_url = get_default_llm_base_url(provider),
  max_records = NULL,
  api_key = get_llm_api_key(provider)
) {
  if (is.null(duckdb_path)) {
    duckdb_path <- get_default_duckdb_path(data_dir)
  }

  inputs <- normalized_data

  if (is.null(inputs)) {
    inputs <- load_pipeline_table("normalized_candidates", rds_path = file.path(data_dir, "normalized_tools.rds"), db_path = duckdb_path, required = TRUE)
  }

  if (!is.data.frame(inputs)) {
    stop("normalized_data must be a data frame.", call. = FALSE)
  }

  previous_results <- .assessment_load_previous_results(output_path, duckdb_path = duckdb_path)
  previous_mitre <- .assessment_load_previous_mitre(mitre_output_path, duckdb_path = duckdb_path)

  reusable_success_ids <- .assessment_find_reusable_successes(inputs, previous_results)
  reused_assessment_rows <- previous_results[previous_results[["record_id"]] %in% reusable_success_ids, , drop = FALSE]
  reused_mitre_rows <- previous_mitre[previous_mitre[["record_id"]] %in% reusable_success_ids, , drop = FALSE]

  pending_inputs <- inputs[!(inputs[["record_id"]] %in% reusable_success_ids), , drop = FALSE]

  candidates <- get_assessment_candidates(
    normalized_data = pending_inputs,
    data_dir = data_dir,
    max_records = max_records
  )

  skipped_inputs <- pending_inputs[!(pending_inputs[["record_id"]] %in% candidates[["record_id"]]), , drop = FALSE]

  if (nrow(candidates) > 0 && !nzchar(api_key)) {
    stop(sprintf("API key for provider '%s' is required.", provider), call. = FALSE)
  }

  log_message(sprintf("Assessment runtime: provider=%s model=%s base_url=%s api_key_present=%s", provider, model, base_url, nzchar(api_key)))
  log_message(sprintf(
    "Assessment queue: total_inputs=%s reused_success=%s pending=%s llm_candidates=%s skipped_pre_filter=%s",
    nrow(inputs),
    length(reusable_success_ids),
    nrow(pending_inputs),
    nrow(candidates),
    nrow(skipped_inputs)
  ))

  assessment_rows <- list()
  mitre_rows <- list()

  for (index in seq_len(nrow(candidates))) {
    record <- candidates[index, , drop = FALSE]
    log_message(sprintf(
      "LLM request started (%s/%s): %s [%s]",
      index,
      nrow(candidates),
      record$name[[1]],
      record$record_id[[1]]
    ))

    result <- tryCatch(
      {
        prompt <- build_unified_tool_assessment_prompt(record)
        json_text <- .openai_chat_completion(
          system_prompt = .assessment_system_prompt(),
          user_prompt = prompt,
          model = model,
          api_key = api_key,
          provider = provider,
          base_url = base_url,
          schema_name = "unified_tool_assessment",
          schema = get_unified_tool_assessment_schema()
        )
        parsed <- parse_unified_tool_assessment_json(json_text, fallback_name = record$name[[1]])
        list(
          assessment = .assessment_success_row(record, parsed, provider = provider, base_url = base_url, model = model),
          mitre = .assessment_mitre_rows(record$record_id[[1]], parsed$assessment$name[[1]], parsed)
        )
      },
      error = function(error) {
        list(
          assessment = .assessment_skipped_row(record, provider = provider, base_url = base_url, llm_status = "error", llm_error = conditionMessage(error)),
          mitre = .assessment_empty_mitre_results()
        )
      }
    )

    log_message(sprintf(
      "LLM request finished (%s/%s): %s => %s",
      index,
      nrow(candidates),
      record$name[[1]],
      result$assessment$llm_status[[1]]
    ))

    assessment_rows[[length(assessment_rows) + 1L]] <- result$assessment
    mitre_rows[[length(mitre_rows) + 1L]] <- result$mitre
  }

  skipped_rows <- lapply(
    seq_len(nrow(skipped_inputs)),
    function(index) {
      .assessment_skipped_row(skipped_inputs[index, , drop = FALSE], provider = provider, base_url = base_url, llm_status = "skipped_pre_filter")
    }
  )

  assessment_results <- dplyr::bind_rows(c(list(reused_assessment_rows), assessment_rows, skipped_rows))
  mitre_mappings <- dplyr::bind_rows(c(list(reused_mitre_rows), mitre_rows))

  if (nrow(assessment_results) == 0) {
    assessment_results <- .assessment_empty_results()
  } else {
    process_values <- as.integer(assessment_results[["pre_llm_should_process"]] %in% TRUE)
    score_values <- assessment_results[["pre_llm_score"]]
    score_values[is.na(score_values)] <- -Inf
    name_values <- assessment_results[["name"]]
    order_index <- order(-process_values, -score_values, name_values, na.last = TRUE)
    assessment_results <- assessment_results[order_index, , drop = FALSE]
  }

  relevant_tools <- assessment_results[
    assessment_results[["llm_status"]] == "success" &
      assessment_results[["is_relevant"]] %in% TRUE &
      assessment_results[["is_tool"]] %in% TRUE,
    ,
    drop = FALSE
  ]

  if (nrow(relevant_tools) > 0) {
    keep_rows <- !vapply(
      seq_len(nrow(relevant_tools)),
      function(index) {
        .normalize_is_documentation_like_candidate(
          name = relevant_tools[["assessed_name"]][[index]],
          raw_text = paste(
            relevant_tools[["summary_ru"]][[index]],
            relevant_tools[["purpose_ru"]][[index]],
            relevant_tools[["reason_ru"]][[index]],
            collapse = " "
          ),
          metadata = relevant_tools[["metadata"]][[index]],
          url = relevant_tools[["url"]][[index]]
        )
      },
      logical(1)
    )

    relevant_tools <- relevant_tools[keep_rows, , drop = FALSE]

    rel_score_values <- relevant_tools[["pre_llm_score"]]
    rel_score_values[is.na(rel_score_values)] <- -Inf
    confidence_values <- relevant_tools[["overall_confidence"]]
    confidence_values[is.na(confidence_values)] <- -Inf
    assessed_names <- relevant_tools[["assessed_name"]]
    order_index <- order(-rel_score_values, -confidence_values, assessed_names, na.last = TRUE)
    relevant_tools <- relevant_tools[order_index, , drop = FALSE]
  }

  save_pipeline_rds(assessment_results, output_path)
  save_pipeline_rds(relevant_tools, relevant_output_path)
  save_pipeline_rds(mitre_mappings, mitre_output_path)

  llm_processing_queue <- build_llm_processing_queue(
    normalized_data = inputs,
    assessment_results = assessment_results,
    data_dir = data_dir,
    output_path = queue_output_path,
    duckdb_path = duckdb_path,
    write_duckdb = write_duckdb,
    run_id = run_id
  )

  if (isTRUE(write_duckdb)) {
    write_duckdb_table(inputs, "normalized_candidates", db_path = duckdb_path, overwrite = TRUE)
    write_pipeline_snapshot_table(assessment_results, "llm_assessments", db_path = duckdb_path, history_table = "llm_assessments_history", run_id = run_id, pipeline_stage = "assessment")
    write_pipeline_snapshot_table(relevant_tools, "relevant_tools", db_path = duckdb_path, history_table = "relevant_tools_history", run_id = run_id, pipeline_stage = "assessment")
    write_pipeline_snapshot_table(mitre_mappings, "mitre_mappings", db_path = duckdb_path, history_table = "mitre_mappings_history", run_id = run_id, pipeline_stage = "assessment")
  }

  log_message(sprintf("Saved %s tool assessment rows to %s", nrow(assessment_results), output_path))
  log_message(sprintf("Saved %s relevant tool rows to %s", nrow(relevant_tools), relevant_output_path))
  log_message(sprintf("Saved %s MITRE mapping rows to %s", nrow(mitre_mappings), mitre_output_path))
  log_message(sprintf("Saved %s LLM queue rows to %s", nrow(llm_processing_queue), queue_output_path))

  list(
    assessment_results = assessment_results,
    relevant_tools = relevant_tools,
    mitre_mappings = mitre_mappings,
    llm_processing_queue = llm_processing_queue
  )
}
