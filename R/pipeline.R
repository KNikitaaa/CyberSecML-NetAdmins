if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".merge_key",
    "pre_llm_should_process",
    "query",
    "query_family",
    "query_min_stars",
    "query_order",
    "query_tier",
    "search_sort"
  ))
}

.pipeline_stage_order <- function(include_validation = FALSE, include_mitre_refinement = FALSE) {
  stages <- c("collect", "normalize")

  if (isTRUE(include_validation)) {
    stages <- c(stages, "validation")
  }

  stages <- c(stages, "assessment")

  if (isTRUE(include_mitre_refinement)) {
    stages <- c(stages, "refine_mitre")
  }

  c(stages, "visualize")
}

.pipeline_timestamp <- function(value = Sys.time()) {
  format(value, "%Y-%m-%d %H:%M:%S")
}

.pipeline_generate_run_id <- function(value = Sys.time()) {
  paste0(format(value, "%Y%m%d-%H%M%S"), "-", sprintf("%04d", sample.int(9999L, 1L)))
}

.pipeline_details_json <- function(value) {
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"))
}

.pipeline_empty_status <- function() {
  tibble::tibble(
    run_id = character(),
    stage = character(),
    status = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    details_json = character(),
    error_message = character()
  )
}

.pipeline_save_status <- function(status_table, status_path, write_duckdb = FALSE, duckdb_path = get_default_duckdb_path(dirname(status_path)), run_id = NA_character_) {
  save_pipeline_rds(status_table, status_path)

  json_path <- sub("[.]rds$", ".json", status_path)
  if (!identical(json_path, status_path)) {
    jsonlite::write_json(status_table, json_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  }

  if (isTRUE(write_duckdb)) {
    write_duckdb_table(status_table, "pipeline_status", db_path = duckdb_path, overwrite = TRUE)
    record_duckdb_snapshot(
      table_name = "pipeline_status",
      row_count = nrow(status_table),
      db_path = duckdb_path,
      run_id = run_id,
      pipeline_stage = "pipeline_status",
      snapshot_scope = "current"
    )
  }

  invisible(status_table)
}

.pipeline_append_status <- function(status_table, stage, status, started_at, finished_at, details = list(), error_message = NA_character_, status_path, run_id = NA_character_, write_duckdb = FALSE, duckdb_path = get_default_duckdb_path(dirname(status_path))) {
  row <- tibble::tibble(
    run_id = as.character(run_id),
    stage = stage,
    status = status,
    started_at = .pipeline_timestamp(started_at),
    finished_at = .pipeline_timestamp(finished_at),
    duration_seconds = round(as.numeric(difftime(finished_at, started_at, units = "secs")), 3),
    details_json = .pipeline_details_json(details),
    error_message = error_message
  )

  updated <- dplyr::bind_rows(status_table, row)

  if (isTRUE(write_duckdb)) {
    append_duckdb_table(row, "pipeline_stage_history", db_path = duckdb_path)
  }

  .pipeline_save_status(updated, status_path, write_duckdb = write_duckdb, duckdb_path = duckdb_path, run_id = run_id)
}

.pipeline_normalize_identity_component <- function(value, dataset_name = NA_character_) {
  normalized <- .normalize_string(value)

  if (is.na(normalized) || !nzchar(normalized)) {
    return("")
  }

  normalized <- if (grepl("^https?://", normalized, ignore.case = TRUE)) {
    .normalize_canonicalize_url(normalized)
  } else {
    tolower(stringr::str_squish(normalized))
  }

  if (identical(dataset_name, "github")) {
    normalized <- sub("^https?://github[.]com/", "", normalized)
    normalized <- sub("^https?://api[.]github[.]com/repos/", "", normalized)
  }

  if (is.na(normalized) || !nzchar(normalized)) {
    return("")
  }

  normalized
}

.pipeline_coalesce_key_columns <- function(df, columns) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(character(0))
  }

  key <- rep("", nrow(df))

  for (column_name in columns) {
    if (!(column_name %in% names(df))) {
      next
    }

    values <- vapply(df[[column_name]], .pipeline_normalize_identity_component, character(1))
    use_value <- !nzchar(key) & nzchar(values)
    key[use_value] <- values[use_value]
  }

  key
}

.pipeline_build_raw_keys <- function(df, dataset_name) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(character(0))
  }

  primary_key <- switch(
    dataset_name,
    github = .pipeline_coalesce_key_columns(df, c("html_url", "api_url", "full_name", "repo")),
    packetstorm = .pipeline_coalesce_key_columns(df, c("url", "guid", "title")),
    rss = .pipeline_coalesce_key_columns(df, c("item_link", "item_guid", "item_title")),
    .pipeline_coalesce_key_columns(df, names(df))
  )

  source_prefix <- if ("source" %in% names(df)) {
    values <- tolower(as.character(df$source))
    values[is.na(values)] <- dataset_name
    values
  } else {
    rep(dataset_name, nrow(df))
  }

  fallback <- paste0("row::", seq_len(nrow(df)))
  key_body <- ifelse(nzchar(primary_key), primary_key, fallback)
  paste(source_prefix, key_body, sep = "::")
}

.pipeline_raw_list_columns <- function(dataset_name) {
  switch(
    dataset_name,
    github = c("topics", "matched_queries", "matched_query_families", "matched_query_tiers", "matched_search_modes", "matched_search_pages"),
    packetstorm = c("tag_names"),
    rss = c("item_categories"),
    character(0)
  )
}

.pipeline_parse_listish_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(character(0))
  }

  if (is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }

  if (length(value) == 0 || all(is.na(value))) {
    return(character(0))
  }

  if (length(value) == 1 && is.character(value)) {
    scalar <- stringr::str_trim(value[[1]])

    if (is.na(scalar) || !nzchar(scalar)) {
      return(character(0))
    }

    parsed <- tryCatch(
      jsonlite::fromJSON(scalar, simplifyVector = TRUE),
      error = function(error) NULL
    )

    if (!is.null(parsed)) {
      if (is.list(parsed)) {
        parsed <- unlist(parsed, recursive = TRUE, use.names = FALSE)
      }

      return(parsed[!is.na(parsed)])
    }

    if (grepl(";", scalar, fixed = TRUE)) {
      parts <- stringr::str_trim(strsplit(scalar, ";", fixed = TRUE)[[1]])
      return(parts[nzchar(parts)])
    }

    return(scalar)
  }

  value <- value[!is.na(value)]

  if (length(value) == 0) {
    return(character(0))
  }

  value
}

.pipeline_harmonize_raw_dataset_schema <- function(df, dataset_name) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }

  list_columns <- intersect(.pipeline_raw_list_columns(dataset_name), names(df))

  if (length(list_columns) == 0) {
    return(tibble::as_tibble(df))
  }

  harmonized <- tibble::as_tibble(df)

  for (column_name in list_columns) {
    harmonized[[column_name]] <- lapply(harmonized[[column_name]], .pipeline_parse_listish_value)
  }

  harmonized
}

.pipeline_merge_raw_dataset <- function(existing, incoming, dataset_name) {
  if (!is.data.frame(existing) || nrow(existing) == 0) {
    return(list(data = incoming, new_rows = if (is.data.frame(incoming)) nrow(incoming) else 0L))
  }

  if (!is.data.frame(incoming) || nrow(incoming) == 0) {
    return(list(data = existing, new_rows = 0L))
  }

  existing <- .pipeline_harmonize_raw_dataset_schema(existing, dataset_name)
  incoming <- .pipeline_harmonize_raw_dataset_schema(incoming, dataset_name)

  incoming_keys <- .pipeline_build_raw_keys(incoming, dataset_name)
  existing_keys <- .pipeline_build_raw_keys(existing, dataset_name)
  new_rows <- sum(!(incoming_keys %in% existing_keys))

  merged <- dplyr::bind_rows(incoming, existing)
  merged_keys <- .pipeline_build_raw_keys(merged, dataset_name)

  merged[[".merge_key"]] <- merged_keys
  merged <- merged[!duplicated(merged[[".merge_key"]]), , drop = FALSE]
  merged[[".merge_key"]] <- NULL

  list(data = merged, new_rows = as.integer(new_rows))
}

.pipeline_empty_sanity_checks <- function() {
  tibble::tibble(
    run_id = character(),
    check_name = character(),
    status = character(),
    severity = character(),
    observed_value = character(),
    expected_value = character(),
    details = character(),
    checked_at = character()
  )
}

.pipeline_add_sanity_check <- function(run_id, check_name, status, severity, observed_value, expected_value, details, checked_at) {
  tibble::tibble(
    run_id = as.character(run_id),
    check_name = as.character(check_name),
    status = as.character(status),
    severity = as.character(severity),
    observed_value = as.character(observed_value),
    expected_value = as.character(expected_value),
    details = as.character(details),
    checked_at = .pipeline_timestamp(checked_at)
  )
}

.pipeline_safe_row_count <- function(value) {
  if (!is.data.frame(value)) {
    return(0L)
  }

  as.integer(nrow(value))
}

.pipeline_duplicate_count <- function(data, column_name) {
  if (!is.data.frame(data) || nrow(data) == 0 || !(column_name %in% names(data))) {
    return(0L)
  }

  values <- as.character(data[[column_name]])
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0) {
    return(0L)
  }

  as.integer(sum(duplicated(values)))
}

.pipeline_run_sanity_checks <- function(data_dir, state, write_duckdb, duckdb_path, run_id = NA_character_) {
  checked_at <- Sys.time()
  normalized_rows <- .pipeline_safe_row_count(state$normalized_data)
  assessment_rows <- .pipeline_safe_row_count(state$assessment_results)
  relevant_rows <- .pipeline_safe_row_count(state$relevant_tools)
  mitre_rows <- .pipeline_safe_row_count(state$mitre_mappings)
  visualization_rows <- .pipeline_safe_row_count(state$visualization_tools)
  normalized_duplicates <- .pipeline_duplicate_count(state$normalized_data, "record_id")
  relevant_duplicates <- .pipeline_duplicate_count(state$relevant_tools, "record_id")
  missing_mitre_ids <- if (is.data.frame(state$mitre_mappings) && nrow(state$mitre_mappings) > 0 && "technique_id" %in% names(state$mitre_mappings)) {
    sum(is.na(state$mitre_mappings[["technique_id"]]) | !nzchar(as.character(state$mitre_mappings[["technique_id"]])))
  } else {
    0L
  }
  orphan_mitre_rows <- if (
    is.data.frame(state$mitre_mappings) &&
    nrow(state$mitre_mappings) > 0 &&
    "record_id" %in% names(state$mitre_mappings) &&
    is.data.frame(state$relevant_tools) &&
    nrow(state$relevant_tools) > 0 &&
    "record_id" %in% names(state$relevant_tools)
  ) {
    sum(!(as.character(state$mitre_mappings[["record_id"]]) %in% as.character(state$relevant_tools[["record_id"]])), na.rm = TRUE)
  } else if (is.data.frame(state$mitre_mappings) && nrow(state$mitre_mappings) > 0) {
    nrow(state$mitre_mappings)
  } else {
    0L
  }
  visualization_materialized <- is.data.frame(state$visualization_tools)
  visualization_status <- if (!visualization_materialized) {
    "pass"
  } else if (relevant_rows == 0L || visualization_rows > 0L) {
    "pass"
  } else {
    "warn"
  }
  visualization_observed <- if (!visualization_materialized) {
    "visualize stage not executed"
  } else {
    sprintf("visualization=%s; relevant=%s", visualization_rows, relevant_rows)
  }
  visualization_expected <- if (!visualization_materialized) {
    "stage optional"
  } else {
    "visualization > 0 when relevant > 0"
  }
  visualization_details <- if (!visualization_materialized) {
    "Visualization check skipped because the visualize stage was not part of this run."
  } else {
    "Visualization layer should materialize when assessment produces relevant tools."
  }

  checks <- dplyr::bind_rows(
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "normalized_non_empty",
      status = if (normalized_rows > 0) "pass" else "fail",
      severity = "critical",
      observed_value = normalized_rows,
      expected_value = "> 0",
      details = "Normalized layer should contain at least one candidate row.",
      checked_at = checked_at
    ),
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "normalized_record_id_unique",
      status = if (normalized_duplicates == 0L) "pass" else "fail",
      severity = "critical",
      observed_value = normalized_duplicates,
      expected_value = "0 duplicates",
      details = "Duplicate normalized record_id values indicate unstable canonical identity.",
      checked_at = checked_at
    ),
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "assessment_covers_relevant_tools",
      status = if (relevant_rows <= assessment_rows || assessment_rows == 0L) "pass" else "fail",
      severity = "critical",
      observed_value = sprintf("relevant=%s; assessed=%s", relevant_rows, assessment_rows),
      expected_value = "relevant <= assessed",
      details = "Relevant tool rows should never exceed assessed rows.",
      checked_at = checked_at
    ),
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "relevant_record_id_unique",
      status = if (relevant_duplicates == 0L) "pass" else "warn",
      severity = "warning",
      observed_value = relevant_duplicates,
      expected_value = "0 duplicates",
      details = "Relevant tool duplicates make run-to-run comparison noisy.",
      checked_at = checked_at
    ),
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "mitre_mapping_has_technique_id",
      status = if (missing_mitre_ids == 0L) "pass" else "warn",
      severity = "warning",
      observed_value = missing_mitre_ids,
      expected_value = "0 missing technique_id",
      details = "MITRE rows without technique_id are not useful for downstream comparison.",
      checked_at = checked_at
    ),
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "mitre_mapping_links_to_relevant_tools",
      status = if (orphan_mitre_rows == 0L) "pass" else "warn",
      severity = "warning",
      observed_value = orphan_mitre_rows,
      expected_value = "0 orphan MITRE rows",
      details = "Every MITRE mapping should point to a relevant tool record_id.",
      checked_at = checked_at
    ),
    .pipeline_add_sanity_check(
      run_id = run_id,
      check_name = "visualization_tracks_relevant_tools",
      status = visualization_status,
      severity = "warning",
      observed_value = visualization_observed,
      expected_value = visualization_expected,
      details = visualization_details,
      checked_at = checked_at
    )
  )

  output_path <- file.path(data_dir, "pipeline_sanity_checks.rds")
  save_pipeline_rds(checks, output_path)
  jsonlite::write_json(checks, path = sub("[.]rds$", ".json", output_path), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")

  if (isTRUE(write_duckdb)) {
    write_pipeline_snapshot_table(
      checks,
      "pipeline_sanity_checks",
      db_path = duckdb_path,
      history_table = "pipeline_sanity_checks_history",
      run_id = run_id,
      pipeline_stage = "sanity_checks"
    )
  }

  failures <- sum(checks$status == "fail", na.rm = TRUE)
  warnings <- sum(checks$status == "warn", na.rm = TRUE)

  list(
    state = list(pipeline_sanity_checks = checks),
    details = list(
      sanity_total_checks = nrow(checks),
      sanity_failed_checks = as.integer(failures),
      sanity_warning_checks = as.integer(warnings),
      sanity_output_path = output_path
    ),
    stage_status = if (failures > 0L || warnings > 0L) "warning" else "success"
  )
}

.pipeline_load_existing_raw_inputs <- function(data_dir, duckdb_path = get_default_duckdb_path(data_dir)) {
  list(
    github = load_pipeline_table("raw_github", rds_path = file.path(data_dir, "raw_github.rds"), db_path = duckdb_path, required = FALSE),
    packetstorm = load_pipeline_table("raw_packetstorm", rds_path = file.path(data_dir, "raw_packetstorm.rds"), db_path = duckdb_path, required = FALSE),
    rss = load_pipeline_table("raw_rss", rds_path = file.path(data_dir, "raw_rss.rds"), db_path = duckdb_path, required = FALSE)
  )
}

.pipeline_github_search_state_path <- function(data_dir) {
  file.path(data_dir, "github_search_state.rds")
}

.pipeline_github_query_log_path <- function(data_dir) {
  file.path(data_dir, "github_search_log.rds")
}

.pipeline_batch_summary_path <- function(data_dir) {
  file.path(data_dir, "pipeline_batch_summary.rds")
}

.pipeline_load_github_search_state <- function(path) {
  state <- load_pipeline_rds(path, required = FALSE)

  if (!is.list(state)) {
    return(list(next_request_offset = 0L, query_plan_size = 0L))
  }

  list(
    next_request_offset = as.integer(state$next_request_offset %||% 0L),
    query_plan_size = as.integer(state$query_plan_size %||% 0L)
  )
}

.pipeline_save_github_search_state <- function(path, next_request_offset, query_plan_size, selected_requests, start_offset) {
  save_pipeline_rds(
    list(
      next_request_offset = as.integer(next_request_offset),
      query_plan_size = as.integer(query_plan_size),
      last_selected_requests = as.integer(selected_requests),
      last_start_offset = as.integer(start_offset),
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    path
  )
}

.pipeline_select_github_query_specs <- function(data_dir, github_queries, github_min_stars, github_sort_modes, github_max_search_requests, rotate_query_plan) {
  query_specs <- .github_normalize_query_specs(
    queries = github_queries,
    default_min_stars = github_min_stars,
    default_sort_modes = github_sort_modes
  )
  query_plan <- .github_build_query_plan(query_specs)
  state_path <- .pipeline_github_search_state_path(data_dir)

  if (nrow(query_plan) == 0) {
    return(list(
      query_specs = query_specs,
      query_plan_size = 0L,
      selected_requests = 0L,
      start_offset = 0L,
      next_request_offset = 0L,
      state_path = state_path
    ))
  }

  selected_plan <- query_plan
  start_offset <- 0L
  next_request_offset <- 0L

  if (isTRUE(rotate_query_plan) && nrow(query_plan) > as.integer(github_max_search_requests)) {
    state <- .pipeline_load_github_search_state(state_path)
    start_offset <- state$next_request_offset %||% 0L
    selected_indexes <- ((start_offset + seq_len(as.integer(github_max_search_requests)) - 1L) %% nrow(query_plan)) + 1L
    selected_plan <- query_plan[selected_indexes, , drop = FALSE]
    next_request_offset <- (start_offset + as.integer(github_max_search_requests)) %% nrow(query_plan)
  }

  selected_group_keys <- paste(
    selected_plan[["query"]],
    selected_plan[["query_family"]],
    selected_plan[["query_tier"]],
    selected_plan[["query_min_stars"]],
    selected_plan[["query_order"]],
    sep = "\r"
  )
  selected_groups <- split(seq_len(nrow(selected_plan)), selected_group_keys)
  selected_specs <- tibble::tibble(
    query = vapply(selected_groups, function(indexes) selected_plan[["query"]][[indexes[[1]]]], character(1)),
    query_family = vapply(selected_groups, function(indexes) selected_plan[["query_family"]][[indexes[[1]]]], character(1)),
    query_tier = vapply(selected_groups, function(indexes) selected_plan[["query_tier"]][[indexes[[1]]]], character(1)),
    query_min_stars = vapply(selected_groups, function(indexes) selected_plan[["query_min_stars"]][[indexes[[1]]]], integer(1)),
    query_order = vapply(selected_groups, function(indexes) selected_plan[["query_order"]][[indexes[[1]]]], integer(1)),
    query_sort_modes = lapply(selected_groups, function(indexes) as.character(selected_plan[["search_sort"]][indexes]))
  )
  selected_specs <- selected_specs[order(selected_specs[["query_order"]]), , drop = FALSE]
  selected_specs[["query_order"]] <- NULL

  list(
    query_specs = selected_specs,
    query_plan_size = nrow(query_plan),
    selected_requests = nrow(selected_plan),
    start_offset = start_offset,
    next_request_offset = next_request_offset,
    state_path = state_path
  )
}

.pipeline_run_collect <- function(data_dir, github_queries, github_min_stars, github_max_results, github_max_search_requests, github_languages, github_include_archived, github_include_readme, github_sort_modes, github_token, packetstorm_categories, rss_feeds, include_linked_content, linked_content_chars, collect_mode = "incremental", write_duckdb = TRUE, duckdb_path = get_default_duckdb_path(data_dir), run_id = NA_character_) {
  existing_raw <- if (identical(collect_mode, "incremental")) .pipeline_load_existing_raw_inputs(data_dir, duckdb_path = duckdb_path) else list(github = NULL, packetstorm = NULL, rss = NULL)
  github_query_log_path <- .pipeline_github_query_log_path(data_dir)
  github_query_selection <- .pipeline_select_github_query_specs(
    data_dir = data_dir,
    github_queries = github_queries,
    github_min_stars = github_min_stars,
    github_sort_modes = github_sort_modes,
    github_max_search_requests = github_max_search_requests,
    rotate_query_plan = identical(collect_mode, "incremental")
  )

  log_message(sprintf(
    "GitHub query rotation: selected_requests=%s plan_size=%s start_offset=%s collect_mode=%s",
    github_query_selection$selected_requests,
    github_query_selection$query_plan_size,
    github_query_selection$start_offset,
    collect_mode
  ))

  github_raw <- collect_github_tools(
    queries = github_query_selection$query_specs,
    min_stars = github_min_stars,
    max_results = github_max_results,
    max_search_requests = github_query_selection$selected_requests,
    languages = github_languages,
    include_archived = github_include_archived,
    include_readme = github_include_readme,
    sort_modes = github_sort_modes,
    token = github_token,
    search_log_path = github_query_log_path,
    output_path = file.path(data_dir, "raw_github.rds")
  )

  github_query_log <- load_pipeline_rds(github_query_log_path, required = FALSE)
  if (!is.data.frame(github_query_log)) {
    github_query_log <- tibble::tibble()
  }

  query_family_text <- paste(unique(github_query_selection$query_specs[["query_family"]]), collapse = "; ")
  query_tier_text <- paste(unique(github_query_selection$query_specs[["query_tier"]]), collapse = "; ")
  next_request_offset <- github_query_selection$next_request_offset
  selection_share_percent <- if (github_query_selection$query_plan_size > 0) {
    round((github_query_selection$selected_requests / github_query_selection$query_plan_size) * 100, 2)
  } else {
    NA_real_
  }
  rotation_progress_percent <- if (github_query_selection$query_plan_size > 0) {
    round((next_request_offset / github_query_selection$query_plan_size) * 100, 2)
  } else {
    NA_real_
  }
  remaining_requests <- if (github_query_selection$query_plan_size > 0) {
    max(github_query_selection$query_plan_size - next_request_offset, 0L)
  } else {
    NA_integer_
  }
  remaining_runs <- if (!is.na(remaining_requests) && github_query_selection$selected_requests > 0) {
    ceiling(remaining_requests / github_query_selection$selected_requests)
  } else {
    NA_integer_
  }

  packetstorm_raw <- scrape_packetstorm(
    categories = packetstorm_categories,
    include_linked_content = include_linked_content,
    linked_content_chars = linked_content_chars,
    output_path = file.path(data_dir, "raw_packetstorm.rds")
  )

  rss_raw <- fetch_security_feeds(
    feed_urls = rss_feeds,
    include_linked_content = include_linked_content,
    linked_content_chars = linked_content_chars,
    output_path = file.path(data_dir, "raw_rss.rds")
  )

  github_merge <- .pipeline_merge_raw_dataset(existing_raw$github, github_raw, "github")
  packetstorm_merge <- .pipeline_merge_raw_dataset(existing_raw$packetstorm, packetstorm_raw, "packetstorm")
  rss_merge <- .pipeline_merge_raw_dataset(existing_raw$rss, rss_raw, "rss")

  if (identical(collect_mode, "incremental")) {
    github_raw <- github_merge$data
    packetstorm_raw <- packetstorm_merge$data
    rss_raw <- rss_merge$data

    log_message(sprintf(
      "Incremental collect merged raw artifacts: github total=%s new=%s | packetstorm total=%s new=%s | rss total=%s new=%s",
      nrow(github_raw),
      github_merge$new_rows,
      nrow(packetstorm_raw),
      packetstorm_merge$new_rows,
      nrow(rss_raw),
      rss_merge$new_rows
    ))

    .pipeline_save_github_search_state(
      path = github_query_selection$state_path,
      next_request_offset = github_query_selection$next_request_offset,
      query_plan_size = github_query_selection$query_plan_size,
      selected_requests = github_query_selection$selected_requests,
      start_offset = github_query_selection$start_offset
    )
  }

  save_pipeline_rds(github_raw, file.path(data_dir, "raw_github.rds"))
  save_pipeline_rds(packetstorm_raw, file.path(data_dir, "raw_packetstorm.rds"))
  save_pipeline_rds(rss_raw, file.path(data_dir, "raw_rss.rds"))

  if (isTRUE(write_duckdb)) {
    write_pipeline_snapshot_table(github_raw, "raw_github", db_path = duckdb_path, history_table = "raw_github_history", run_id = run_id, pipeline_stage = "collect")
    write_pipeline_snapshot_table(packetstorm_raw, "raw_packetstorm", db_path = duckdb_path, history_table = "raw_packetstorm_history", run_id = run_id, pipeline_stage = "collect")
    write_pipeline_snapshot_table(rss_raw, "raw_rss", db_path = duckdb_path, history_table = "raw_rss_history", run_id = run_id, pipeline_stage = "collect")
    write_pipeline_snapshot_table(github_query_log, "github_search_log", db_path = duckdb_path, history_table = "github_search_log_history", run_id = run_id, pipeline_stage = "collect")
  }

  list(
    state = list(
      raw_inputs = list(github = github_raw, packetstorm = packetstorm_raw, rss = rss_raw)
    ),
    details = list(
      collect_mode = collect_mode,
      github_rows = nrow(github_raw),
      github_new_rows = if (identical(collect_mode, "incremental")) github_merge$new_rows else nrow(github_raw),
      github_query_plan_size = github_query_selection$query_plan_size,
      github_selected_requests = github_query_selection$selected_requests,
      github_request_offset = github_query_selection$start_offset,
      github_next_request_offset = next_request_offset,
      github_selection_share_percent = selection_share_percent,
      github_rotation_progress_percent = rotation_progress_percent,
      github_remaining_runs_estimate = remaining_runs,
      github_remaining_request_slots = remaining_requests,
      github_logged_requests = nrow(github_query_log),
      github_logged_query_families = query_family_text,
      github_logged_query_tiers = query_tier_text,
      storage_backend = if (isTRUE(write_duckdb)) "duckdb_primary" else "rds_only",
      packetstorm_rows = nrow(packetstorm_raw),
      packetstorm_new_rows = if (identical(collect_mode, "incremental")) packetstorm_merge$new_rows else nrow(packetstorm_raw),
      rss_rows = nrow(rss_raw),
      rss_new_rows = if (identical(collect_mode, "incremental")) rss_merge$new_rows else nrow(rss_raw)
    )
  )
}

.pipeline_run_normalize <- function(data_dir, raw_inputs, deduplicate, write_duckdb, duckdb_path, run_id) {
  normalized <- normalize_raw_data(
    raw_list = raw_inputs,
    data_dir = data_dir,
    deduplicate = deduplicate,
    output_path = file.path(data_dir, "normalized_tools.rds"),
    duckdb_path = duckdb_path,
    write_duckdb = write_duckdb,
    run_id = run_id
  )

  list(
    state = list(normalized_data = normalized),
    details = list(normalized_rows = nrow(normalized))
  )
}

.pipeline_run_validation <- function(data_dir, normalized_data, provider, model, base_url, max_records, api_key) {
  result <- run_validation_enrichment(
    normalized_data = normalized_data,
    data_dir = data_dir,
    output_path = file.path(data_dir, "validation_enrichment_results.rds"),
    enriched_output_path = file.path(data_dir, "enriched_tools.rds"),
    provider = provider,
    model = model,
    base_url = base_url,
    max_records = max_records,
    api_key = api_key
  )

  list(
    state = list(
      validation_results = result$validation_results,
      enriched_tools = result$enriched_tools
    ),
    details = list(
      validation_rows = nrow(result$validation_results),
      enriched_rows = nrow(result$enriched_tools)
    )
  )
}

.pipeline_run_assessment <- function(data_dir, normalized_data, provider, model, base_url, max_records, api_key, write_duckdb, duckdb_path, run_id = NA_character_) {
  result <- run_unified_tool_assessment(
    normalized_data = normalized_data,
    data_dir = data_dir,
    output_path = file.path(data_dir, "tool_assessment_results.rds"),
    relevant_output_path = file.path(data_dir, "relevant_tools.rds"),
    mitre_output_path = file.path(data_dir, "tool_mitre_mappings.rds"),
    duckdb_path = duckdb_path,
    write_duckdb = write_duckdb,
    run_id = run_id,
    provider = provider,
    model = model,
    base_url = base_url,
    max_records = max_records,
    api_key = api_key
  )

  list(
    state = list(
      assessment_results = result$assessment_results,
      relevant_tools = result$relevant_tools,
      mitre_mappings = result$mitre_mappings
    ),
    details = list(
      assessment_rows = nrow(result$assessment_results),
      relevant_rows = nrow(result$relevant_tools),
      mitre_rows = nrow(result$mitre_mappings)
    )
  )
}

.pipeline_run_visualize <- function(data_dir, assessment_results, mitre_mappings, write_duckdb, duckdb_path) {
  result <- build_visualization_dataset(
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    data_dir = data_dir,
    output_path = file.path(data_dir, "visualization_tools.rds"),
    matrix_output_path = file.path(data_dir, "visualization_tool_matrix.rds"),
    duckdb_path = duckdb_path,
    write_duckdb = write_duckdb
  )

  list(
    state = list(
      visualization_tools = result$visualization_tools,
      visualization_tool_matrix = result$visualization_tool_matrix
    ),
    details = list(
      visualization_rows = nrow(result$visualization_tools),
      matrix_rows = nrow(result$visualization_tool_matrix)
    )
  )
}

.pipeline_run_refinement <- function(data_dir, assessment_results, mitre_mappings, refinement_index_path, refinement_output_path, top_k, min_score, overwrite_index, mitre_attack, mitre_attack_path) {
  result <- run_mitre_refinement(
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    mitre_attack = mitre_attack,
    data_dir = data_dir,
    index_path = refinement_index_path,
    output_path = refinement_output_path,
    top_k = top_k,
    min_score = min_score,
    overwrite_index = overwrite_index,
    mitre_attack_path = mitre_attack_path
  )

  list(
    state = list(mitre_refinement_candidates = result),
    details = list(refinement_rows = nrow(result))
  )
}

.pipeline_run_existing_backlog <- function(
  data_dir,
  status_table,
  status_path,
  run_id,
  provider,
  model,
  base_url,
  max_records,
  api_key,
  write_duckdb,
  duckdb_path,
  run_mitre_refinement,
  refinement_index_path,
  refinement_output_path,
  refinement_top_k,
  refinement_min_score,
  overwrite_refinement_index,
  mitre_attack,
  mitre_attack_path
) {
  normalized_path <- file.path(data_dir, "normalized_tools.rds")
  normalized_data <- load_pipeline_table("normalized_candidates", rds_path = normalized_path, db_path = duckdb_path, required = FALSE)

  if (!is.data.frame(normalized_data) || nrow(normalized_data) == 0) {
    log_message("Backlog preflight skipped: normalized_tools.rds is missing or empty")
    return(list(status_table = status_table, state = list()))
  }

  backlog_candidates <- normalized_data[normalized_data[["pre_llm_should_process"]] %in% TRUE, , drop = FALSE]

  if (nrow(backlog_candidates) == 0) {
    log_message("Backlog preflight skipped: no pre-filter-approved candidates in existing normalized data")
    return(list(status_table = status_table, state = list()))
  }

  log_message(sprintf("Backlog preflight started for %s existing normalized candidates", nrow(backlog_candidates)))

  state <- list(
    normalized_data = normalized_data,
    assessment_results = NULL,
    relevant_tools = NULL,
    mitre_mappings = NULL,
    visualization_tools = NULL,
    visualization_tool_matrix = NULL,
    mitre_refinement_candidates = NULL
  )

  preflight_stages <- c("preflight_assessment")
  if (isTRUE(run_mitre_refinement)) {
    preflight_stages <- c(preflight_stages, "preflight_refine_mitre")
  }
  preflight_stages <- c(preflight_stages, "preflight_visualize")

  for (current_stage in preflight_stages) {
    started_at <- Sys.time()
    log_message(sprintf("Pipeline stage started: %s", current_stage))

    stage_result <- tryCatch(
      switch(
        current_stage,
        preflight_assessment = .pipeline_run_assessment(
          data_dir = data_dir,
          normalized_data = state$normalized_data,
          provider = provider,
          model = model,
          base_url = base_url,
          max_records = max_records,
          api_key = api_key,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path,
          run_id = run_id
        ),
        preflight_refine_mitre = .pipeline_run_refinement(
          data_dir = data_dir,
          assessment_results = state$assessment_results,
          mitre_mappings = state$mitre_mappings,
          refinement_index_path = refinement_index_path,
          refinement_output_path = refinement_output_path,
          top_k = refinement_top_k,
          min_score = refinement_min_score,
          overwrite_index = overwrite_refinement_index,
          mitre_attack = mitre_attack,
          mitre_attack_path = mitre_attack_path
        ),
        preflight_visualize = .pipeline_run_visualize(
          data_dir = data_dir,
          assessment_results = state$assessment_results,
          mitre_mappings = state$mitre_mappings,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path
        ),
        stop(sprintf("Unsupported backlog preflight stage '%s'.", current_stage), call. = FALSE)
      ),
      error = function(error) {
        finished_at <- Sys.time()
        updated_status <- .pipeline_append_status(
          status_table = status_table,
          stage = current_stage,
          status = "error",
          started_at = started_at,
          finished_at = finished_at,
          details = list(),
          error_message = conditionMessage(error),
          status_path = status_path,
          run_id = run_id,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path
        )
        status_table <<- updated_status
        stop(error)
      }
    )

    for (name in names(stage_result$state)) {
      state[[name]] <- stage_result$state[[name]]
    }

    finished_at <- Sys.time()
    status_table <- .pipeline_append_status(
      status_table = status_table,
      stage = current_stage,
      status = "success",
      started_at = started_at,
      finished_at = finished_at,
      details = c(list(backlog_rows = nrow(backlog_candidates)), stage_result$details),
      error_message = NA_character_,
      status_path = status_path,
      run_id = run_id,
      write_duckdb = write_duckdb,
      duckdb_path = duckdb_path
    )

    log_message(sprintf("Pipeline stage finished: %s", current_stage))
  }

  list(status_table = status_table, state = state)
}

#' Run the complete OffensiveToolMapper pipeline
#'
#' @inheritParams run_pipeline_from
#'
#' @return Named list with pipeline outputs and stage status table.
run_full_pipeline <- function(...) {
  run_pipeline_from(stage = "collect", ...)
}

#' Run the OffensiveToolMapper pipeline from a specific stage
#'
#' @param stage Start stage. One of `collect`, `normalize`, `validation`, `assessment`, `refine_mitre`, `visualize`.
#' @param end_stage Optional final stage to stop at.
#' @param data_dir Directory for pipeline artifacts.
#' @param status_path Path where pipeline stage status is persisted.
#' @param raw_inputs Optional named list with `github`, `packetstorm`, and `rss` raw data.
#' @param normalized_data Optional normalized candidate table.
#' @param assessment_results Optional unified assessment results.
#' @param mitre_mappings Optional MITRE mapping table.
#' @param visualization_tools Optional UI-facing visualization tools.
#' @param visualization_tool_matrix Optional UI-facing visualization matrix.
#' @param github_queries GitHub search queries or structured query specs.
#' @param github_min_stars Minimum GitHub stars used as a global floor.
#' @param github_max_results Maximum GitHub rows.
#' @param github_max_search_requests Maximum GitHub search requests per pipeline run.
#' @param github_languages Optional GitHub languages filter.
#' @param github_include_archived Whether archived repositories may be returned.
#' @param github_include_readme Whether README content should be fetched.
#' @param github_sort_modes GitHub sort modes.
#' @param github_token Optional GitHub token override.
#' @param packetstorm_categories Packet Storm News sections. Requests require
#'   `PACKETSTORM_API_SECRET`.
#' @param rss_feeds RSS feeds to collect. Defaults to disabled.
#' @param include_linked_content Whether RSS and Packet Storm linked pages
#'   should be fetched.
#' @param linked_content_chars Maximum linked content size.
#' @param collect_mode Collection mode. Use `incremental` to merge newly collected raw rows into existing artifacts, or `snapshot` to replace them on each run.
#' @param deduplicate Whether normalization should deduplicate candidates.
#' @param run_legacy_validation Whether to include the legacy validation stage.
#' @param provider LLM provider.
#' @param model LLM model.
#' @param base_url LLM base URL.
#' @param max_records Optional limit on LLM candidate processing.
#' @param api_key LLM API key.
#' @param process_existing_backlog Whether to assess and publish the current normalized backlog before running a fresh collection pass.
#' @param write_duckdb Whether DuckDB outputs should be written.
#' @param duckdb_path DuckDB database path.
#' @param run_mitre_refinement Whether to generate retrieval-based MITRE refinement suggestions.
#' @param refinement_top_k Maximum number of refinement candidates per tool.
#' @param refinement_min_score Minimum retrieval score to keep.
#' @param refinement_index_path Output path for the cached MITRE refinement index.
#' @param refinement_output_path Output path for MITRE refinement candidates.
#' @param overwrite_refinement_index Whether to rebuild the cached refinement index.
#' @param mitre_attack Optional MITRE ATT&CK matrix.
#' @param mitre_attack_path Optional path to `data/mitre_attack.rda`.
#'
#' @return Named list with intermediate outputs and the persisted `pipeline_status` table.
run_pipeline_from <- function(
  stage = c("collect", "normalize", "validation", "assessment", "refine_mitre", "visualize"),
  end_stage = NULL,
  data_dir = get_default_data_dir(),
  status_path = file.path(data_dir, "pipeline_status.rds"),
  raw_inputs = NULL,
  normalized_data = NULL,
  assessment_results = NULL,
  mitre_mappings = NULL,
  visualization_tools = NULL,
  visualization_tool_matrix = NULL,
  github_queries = get_default_github_query_specs(),
  github_min_stars = 3L,
  github_max_results = 100L,
  github_max_search_requests = 60L,
  github_languages = NULL,
  github_include_archived = FALSE,
  github_include_readme = TRUE,
  github_sort_modes = get_default_github_search_modes(),
  github_token = NULL,
  packetstorm_categories = get_default_packetstorm_categories(),
  rss_feeds = get_default_feeds(),
  include_linked_content = TRUE,
  linked_content_chars = 2500L,
  collect_mode = c("incremental", "snapshot"),
  deduplicate = TRUE,
  run_legacy_validation = FALSE,
  provider = get_default_llm_provider(),
  model = get_default_llm_model(provider),
  base_url = get_default_llm_base_url(provider),
  max_records = get_default_llm_max_records(),
  api_key = get_llm_api_key(provider),
  process_existing_backlog = TRUE,
  write_duckdb = TRUE,
  duckdb_path = get_default_duckdb_path(data_dir),
  run_mitre_refinement = FALSE,
  refinement_top_k = 8L,
  refinement_min_score = 0.12,
  refinement_index_path = file.path(data_dir, "mitre_refinement_index.rds"),
  refinement_output_path = file.path(data_dir, "mitre_refinement_candidates.rds"),
  overwrite_refinement_index = FALSE,
  mitre_attack = NULL,
  mitre_attack_path = NULL
) {
  ensure_dir(data_dir)
  collect_mode <- match.arg(collect_mode)
  run_id <- .pipeline_generate_run_id()

  available_stages <- .pipeline_stage_order(
    include_validation = run_legacy_validation,
    include_mitre_refinement = run_mitre_refinement
  )

  stage <- match.arg(stage, available_stages)
  if (!is.null(end_stage)) {
    end_stage <- match.arg(end_stage, available_stages)
  }

  start_index <- match(stage, available_stages)
  end_index <- if (is.null(end_stage)) length(available_stages) else match(end_stage, available_stages)

  if (start_index > end_index) {
    stop("end_stage must come after or equal to stage.", call. = FALSE)
  }

  log_message(sprintf("Running pipeline from stage '%s' to '%s' (run_id=%s)", stage, available_stages[[end_index]], run_id))

  status_table <- .pipeline_empty_status()
  .pipeline_save_status(status_table, status_path, write_duckdb = write_duckdb, duckdb_path = duckdb_path, run_id = run_id)

  state <- list(
    raw_inputs = raw_inputs,
    normalized_data = normalized_data,
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    visualization_tools = visualization_tools,
    visualization_tool_matrix = visualization_tool_matrix,
    validation_results = NULL,
    enriched_tools = NULL,
    relevant_tools = NULL,
    mitre_refinement_candidates = NULL,
    pipeline_sanity_checks = NULL
  )

  if (identical(stage, "collect") && isTRUE(process_existing_backlog)) {
    preflight_result <- .pipeline_run_existing_backlog(
      data_dir = data_dir,
      status_table = status_table,
      status_path = status_path,
      run_id = run_id,
      provider = provider,
      model = model,
      base_url = base_url,
      max_records = max_records,
      api_key = api_key,
      write_duckdb = write_duckdb,
      duckdb_path = duckdb_path,
      run_mitre_refinement = run_mitre_refinement,
      refinement_index_path = refinement_index_path,
      refinement_output_path = refinement_output_path,
      refinement_top_k = refinement_top_k,
      refinement_min_score = refinement_min_score,
      overwrite_refinement_index = overwrite_refinement_index,
      mitre_attack = mitre_attack,
      mitre_attack_path = mitre_attack_path
    )

    status_table <- preflight_result$status_table
  }

  for (current_stage in available_stages[start_index:end_index]) {
    started_at <- Sys.time()
    log_message(sprintf("Pipeline stage started: %s", current_stage))

    stage_result <- tryCatch(
      switch(
        current_stage,
        collect = .pipeline_run_collect(
          data_dir = data_dir,
          github_queries = github_queries,
          github_min_stars = github_min_stars,
          github_max_results = github_max_results,
          github_max_search_requests = github_max_search_requests,
          github_languages = github_languages,
          github_include_archived = github_include_archived,
          github_include_readme = github_include_readme,
          github_sort_modes = github_sort_modes,
          github_token = github_token,
          packetstorm_categories = packetstorm_categories,
          rss_feeds = rss_feeds,
          include_linked_content = include_linked_content,
          linked_content_chars = linked_content_chars,
          collect_mode = collect_mode,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path,
          run_id = run_id
        ),
        normalize = .pipeline_run_normalize(
          data_dir = data_dir,
          raw_inputs = state$raw_inputs,
          deduplicate = deduplicate,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path,
          run_id = run_id
        ),
        validation = .pipeline_run_validation(
          data_dir = data_dir,
          normalized_data = state$normalized_data,
          provider = provider,
          model = model,
          base_url = base_url,
          max_records = max_records,
          api_key = api_key
        ),
        assessment = .pipeline_run_assessment(
          data_dir = data_dir,
          normalized_data = state$normalized_data,
          provider = provider,
          model = model,
          base_url = base_url,
          max_records = max_records,
          api_key = api_key,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path,
          run_id = run_id
        ),
        refine_mitre = .pipeline_run_refinement(
          data_dir = data_dir,
          assessment_results = state$assessment_results,
          mitre_mappings = state$mitre_mappings,
          refinement_index_path = refinement_index_path,
          refinement_output_path = refinement_output_path,
          top_k = refinement_top_k,
          min_score = refinement_min_score,
          overwrite_index = overwrite_refinement_index,
          mitre_attack = mitre_attack,
          mitre_attack_path = mitre_attack_path
        ),
        visualize = .pipeline_run_visualize(
          data_dir = data_dir,
          assessment_results = state$assessment_results,
          mitre_mappings = state$mitre_mappings,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path
        ),
        stop(sprintf("Unsupported pipeline stage '%s'.", current_stage), call. = FALSE)
      ),
      error = function(error) {
        finished_at <- Sys.time()
        updated_status <- .pipeline_append_status(
          status_table = status_table,
          stage = current_stage,
          status = "error",
          started_at = started_at,
          finished_at = finished_at,
          details = list(),
          error_message = conditionMessage(error),
          status_path = status_path,
          run_id = run_id,
          write_duckdb = write_duckdb,
          duckdb_path = duckdb_path
        )
        status_table <<- updated_status
        stop(error)
      }
    )

    for (name in names(stage_result$state)) {
      state[[name]] <- stage_result$state[[name]]
    }

    finished_at <- Sys.time()
    status_table <- .pipeline_append_status(
      status_table = status_table,
      stage = current_stage,
      status = "success",
      started_at = started_at,
      finished_at = finished_at,
      details = stage_result$details,
      error_message = NA_character_,
      status_path = status_path,
      run_id = run_id,
      write_duckdb = write_duckdb,
      duckdb_path = duckdb_path
    )

    log_message(sprintf("Pipeline stage finished: %s", current_stage))
  }

  sanity_started_at <- Sys.time()
  log_message("Pipeline stage started: sanity_checks")

  sanity_result <- tryCatch(
    .pipeline_run_sanity_checks(
      data_dir = data_dir,
      state = state,
      write_duckdb = write_duckdb,
      duckdb_path = duckdb_path,
      run_id = run_id
    ),
    error = function(error) {
      finished_at <- Sys.time()
      updated_status <- .pipeline_append_status(
        status_table = status_table,
        stage = "sanity_checks",
        status = "error",
        started_at = sanity_started_at,
        finished_at = finished_at,
        details = list(),
        error_message = conditionMessage(error),
        status_path = status_path,
        run_id = run_id,
        write_duckdb = write_duckdb,
        duckdb_path = duckdb_path
      )
      status_table <<- updated_status
      stop(error)
    }
  )

  for (name in names(sanity_result$state)) {
    state[[name]] <- sanity_result$state[[name]]
  }

  sanity_finished_at <- Sys.time()
  status_table <- .pipeline_append_status(
    status_table = status_table,
    stage = "sanity_checks",
    status = sanity_result$stage_status,
    started_at = sanity_started_at,
    finished_at = sanity_finished_at,
    details = sanity_result$details,
    error_message = NA_character_,
    status_path = status_path,
    run_id = run_id,
    write_duckdb = write_duckdb,
    duckdb_path = duckdb_path
  )

  log_message(sprintf("Pipeline stage finished: %s", "sanity_checks"))

  state$pipeline_status <- status_table
  state
}
