get_default_duckdb_path <- function(data_dir = get_default_data_dir()) {
  file.path(data_dir, "offensive_tool_mapper.duckdb")
}

.duckdb_json_columns_by_table <- function() {
  list(
    raw_github = c("topics", "matched_queries", "matched_query_families", "matched_query_tiers", "matched_search_modes", "matched_search_pages"),
    raw_github_history = c("topics", "matched_queries", "matched_query_families", "matched_query_tiers", "matched_search_modes", "matched_search_pages"),
    raw_packetstorm = c("tag_names"),
    raw_packetstorm_history = c("tag_names"),
    raw_rss = c("item_categories"),
    raw_rss_history = c("item_categories"),
    normalized_candidates = c("pre_llm_reasons", "metadata"),
    normalized_candidates_history = c("pre_llm_reasons", "metadata"),
    llm_assessments = c("pre_llm_reasons", "capabilities_ru", "target_platforms", "mitre_tactics", "mitre_technique_ids", "mitre_technique_names", "filter_tags", "metadata", "mitre_matrix"),
    llm_assessments_history = c("pre_llm_reasons", "capabilities_ru", "target_platforms", "mitre_tactics", "mitre_technique_ids", "mitre_technique_names", "filter_tags", "metadata", "mitre_matrix"),
    relevant_tools = c("pre_llm_reasons", "capabilities_ru", "target_platforms", "mitre_tactics", "mitre_technique_ids", "mitre_technique_names", "filter_tags", "metadata", "mitre_matrix"),
    relevant_tools_history = c("pre_llm_reasons", "capabilities_ru", "target_platforms", "mitre_tactics", "mitre_technique_ids", "mitre_technique_names", "filter_tags", "metadata", "mitre_matrix"),
    llm_processing_queue = c("pre_llm_reasons", "metadata"),
    llm_processing_queue_history = c("pre_llm_reasons", "metadata"),
    visualization_tools = c("capabilities_ru", "mitre_tactics", "mitre_technique_ids", "mitre_technique_names", "filter_tags", "mitre_matrix"),
    visualization_tool_history = c("capabilities_ru", "mitre_tactics", "mitre_technique_ids", "mitre_technique_names", "filter_tags", "mitre_matrix")
  )
}

.duckdb_json_columns_for_table <- function(table_name) {
  columns <- .duckdb_json_columns_by_table()[[table_name]]

  if (is.null(columns)) {
    return(character(0))
  }

  columns
}

.duckdb_prepare_value <- function(value) {
  if (is.null(value)) {
    return(NA_character_)
  }

  if (length(value) == 0) {
    return(NA_character_)
  }

  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
}

.duckdb_prepare_table <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(data)
  }

  prepared <- data
  list_columns <- vapply(prepared, is.list, logical(1))

  for (column_name in names(prepared)[list_columns]) {
    prepared[[column_name]] <- vapply(prepared[[column_name]], .duckdb_prepare_value, character(1))
  }

  prepared
}

.duckdb_restore_value <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }

  value <- as.character(value[[1]])

  if (is.na(value) || !nzchar(value)) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(value, simplifyVector = TRUE),
    error = function(error) {
      value
    }
  )
}

.duckdb_restore_table <- function(data, table_name) {
  if (!is.data.frame(data)) {
    return(data)
  }

  restored <- tibble::as_tibble(data)
  json_columns <- intersect(.duckdb_json_columns_for_table(table_name), names(restored))

  for (column_name in json_columns) {
    restored[[column_name]] <- lapply(restored[[column_name]], .duckdb_restore_value)
  }

  restored
}

.duckdb_connect <- function(db_path, read_only = FALSE) {
  ensure_dir(dirname(db_path))
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

duckdb_table_exists <- function(table_name, db_path = get_default_duckdb_path()) {
  if (!file.exists(db_path)) {
    return(FALSE)
  }

  connection <- .duckdb_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  table_name %in% DBI::dbListTables(connection)
}

init_pipeline_duckdb <- function(db_path = get_default_duckdb_path()) {
  ensure_dir(dirname(db_path))
  connection <- .duckdb_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  invisible(db_path)
}

write_duckdb_table <- function(data, table_name, db_path = get_default_duckdb_path(), overwrite = TRUE) {
  ensure_dir(dirname(db_path))
  connection <- .duckdb_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  prepared <- .duckdb_prepare_table(data)
  DBI::dbWriteTable(connection, name = table_name, value = prepared, overwrite = overwrite)
  invisible(table_name)
}

append_duckdb_table <- function(data, table_name, db_path = get_default_duckdb_path()) {
  if (!is.data.frame(data)) {
    stop("DuckDB append expects a data frame.", call. = FALSE)
  }

  prepared <- .duckdb_prepare_table(data)
  connection <- .duckdb_connect(db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  if (!(table_name %in% DBI::dbListTables(connection))) {
    DBI::dbWriteTable(connection, name = table_name, value = prepared, overwrite = TRUE)
    return(invisible(table_name))
  }

  DBI::dbAppendTable(connection, name = table_name, value = prepared)
  invisible(table_name)
}

read_duckdb_table <- function(table_name, db_path = get_default_duckdb_path(), required = TRUE, restore = TRUE) {
  if (!file.exists(db_path)) {
    if (isTRUE(required)) {
      stop(sprintf("DuckDB file not found: %s", db_path), call. = FALSE)
    }

    return(NULL)
  }

  connection <- .duckdb_connect(db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  if (!(table_name %in% DBI::dbListTables(connection))) {
    if (isTRUE(required)) {
      stop(sprintf("DuckDB table not found: %s", table_name), call. = FALSE)
    }

    return(NULL)
  }

  data <- DBI::dbReadTable(connection, table_name)

  if (!isTRUE(restore)) {
    return(tibble::as_tibble(data))
  }

  .duckdb_restore_table(data, table_name)
}

load_pipeline_table <- function(table_name, rds_path = NULL, db_path = NULL, required = FALSE) {
  if (is.null(db_path)) {
    if (!is.null(rds_path)) {
      db_path <- get_default_duckdb_path(dirname(rds_path))
    } else {
      db_path <- get_default_duckdb_path()
    }
  }

  duckdb_value <- read_duckdb_table(table_name, db_path = db_path, required = FALSE, restore = TRUE)
  if (is.data.frame(duckdb_value)) {
    return(duckdb_value)
  }

  if (!is.null(rds_path) && file.exists(rds_path)) {
    return(load_pipeline_rds(rds_path, required = required))
  }

  if (isTRUE(required)) {
    stop(sprintf("Required dataset '%s' was not found in DuckDB or RDS fallback.", table_name), call. = FALSE)
  }

  NULL
}

record_duckdb_snapshot <- function(table_name, row_count, db_path = get_default_duckdb_path(), run_id = NA_character_, pipeline_stage = NA_character_, snapshot_scope = "current") {
  snapshot_row <- tibble::tibble(
    run_id = as.character(run_id),
    pipeline_stage = as.character(pipeline_stage),
    table_name = as.character(table_name),
    snapshot_scope = as.character(snapshot_scope),
    row_count = as.integer(row_count),
    stored_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  append_duckdb_table(snapshot_row, "pipeline_storage_snapshots", db_path = db_path)
}

write_pipeline_snapshot_table <- function(data, table_name, db_path = get_default_duckdb_path(), history_table = NULL, run_id = NA_character_, pipeline_stage = NA_character_) {
  init_pipeline_duckdb(db_path)
  write_duckdb_table(data, table_name, db_path = db_path, overwrite = TRUE)
  record_duckdb_snapshot(
    table_name = table_name,
    row_count = if (is.data.frame(data)) nrow(data) else 0L,
    db_path = db_path,
    run_id = run_id,
    pipeline_stage = pipeline_stage,
    snapshot_scope = "current"
  )

  if (!is.null(history_table) && is.data.frame(data) && nrow(data) > 0) {
    history_rows <- tibble::as_tibble(data)
    history_rows[["run_id"]] <- rep(as.character(run_id), nrow(history_rows))
    history_rows[["pipeline_stage"]] <- rep(as.character(pipeline_stage), nrow(history_rows))
    history_rows[["stored_at"]] <- rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), nrow(history_rows))
    append_duckdb_table(history_rows, history_table, db_path = db_path)
  }

  invisible(table_name)
}