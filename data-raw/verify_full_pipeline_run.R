project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "etl_github.R"))
source(file.path(project_root, "R", "etl_packetstorm.R"))
source(file.path(project_root, "R", "etl_rss.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "llm_validation.R"))
source(file.path(project_root, "R", "storage_duckdb.R"))
source(file.path(project_root, "R", "llm_assessment.R"))
source(file.path(project_root, "R", "visualization_data.R"))
source(file.path(project_root, "R", "rag_refinement.R"))
source(file.path(project_root, "R", "pipeline.R"))

data_dir <- file.path(project_root, "inst", "extdata")
duckdb_path <- get_default_duckdb_path(data_dir)

select_previous_run_id <- function(db_path) {
  stage_history <- read_duckdb_table("pipeline_stage_history", db_path = db_path, required = FALSE, restore = FALSE)

  if (!is.data.frame(stage_history) || nrow(stage_history) == 0 || !("run_id" %in% names(stage_history))) {
    return(NA_character_)
  }

  stage_history <- stage_history[!is.na(stage_history[["run_id"]]) & nzchar(stage_history[["run_id"]]), , drop = FALSE]

  if (nrow(stage_history) == 0) {
    return(NA_character_)
  }

  stage_history <- stage_history[order(stage_history[["finished_at"]], stage_history[["started_at"]]), , drop = FALSE]
  run_ids <- unique(as.character(stage_history[["run_id"]]))

  if (length(run_ids) == 0) {
    return(NA_character_)
  }

  utils::tail(run_ids, 1L)
}

run_row_count <- function(data, run_id) {
  if (!is.data.frame(data) || nrow(data) == 0 || is.na(run_id) || !nzchar(run_id) || !("run_id" %in% names(data))) {
    return(0L)
  }

  as.integer(sum(as.character(data[["run_id"]]) == run_id, na.rm = TRUE))
}

run_unique_keys <- function(data, run_id, columns) {
  if (!is.data.frame(data) || nrow(data) == 0 || is.na(run_id) || !nzchar(run_id) || !("run_id" %in% names(data))) {
    return(character(0))
  }

  rows <- data[as.character(data[["run_id"]]) == run_id, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(character(0))
  }

  values <- apply(rows[, columns, drop = FALSE], 1, function(entry) paste(as.character(entry), collapse = "::"))
  unique(values[!is.na(values) & nzchar(values)])
}

previous_run_id <- if (file.exists(duckdb_path)) select_previous_run_id(duckdb_path) else NA_character_
provider <- get_default_llm_provider()
runtime_config <- get_llm_runtime_config(provider)
max_records <- get_default_llm_max_records()
collect_mode <- match.arg(
  get_runtime_env_value("OTM_COLLECT_MODE", unset = "incremental"),
  choices = c("incremental", "snapshot")
)
github_min_stars <- as.integer(get_runtime_env_value("OTM_GITHUB_MIN_STARS", unset = "0"))
github_max_results <- as.integer(get_runtime_env_value("OTM_GITHUB_MAX_RESULTS", unset = "30"))
github_max_search_requests <- as.integer(get_runtime_env_value("OTM_GITHUB_MAX_SEARCH_REQUESTS", unset = "1"))

log_message(sprintf(
  "Verification run preflight: previous_run_id=%s provider=%s model=%s base_url=%s api_key_present=%s collect_mode=%s",
  ifelse(is.na(previous_run_id), "none", previous_run_id),
  runtime_config$provider[[1]],
  runtime_config$model[[1]],
  runtime_config$base_url[[1]],
  runtime_config$api_key_present[[1]],
  collect_mode
))

result <- run_full_pipeline(
  data_dir = data_dir,
  provider = provider,
  model = runtime_config$model[[1]],
  base_url = runtime_config$base_url[[1]],
  max_records = max_records,
  api_key = get_llm_api_key(provider),
  collect_mode = collect_mode,
  github_min_stars = github_min_stars,
  github_max_results = github_max_results,
  github_max_search_requests = github_max_search_requests,
  run_mitre_refinement = TRUE,
  mitre_attack_path = file.path(project_root, "data", "mitre_attack.rda")
)

status_table <- result$pipeline_status
current_run_id <- utils::tail(stats::na.omit(as.character(status_table[["run_id"]])), 1L)
current_run_id <- if (length(current_run_id) == 0) NA_character_ else current_run_id[[1]]

relevant_history <- read_duckdb_table("relevant_tools_history", db_path = duckdb_path, required = FALSE, restore = TRUE)
mitre_history <- read_duckdb_table("mitre_mappings_history", db_path = duckdb_path, required = FALSE, restore = TRUE)
sanity_checks <- load_pipeline_rds(file.path(data_dir, "pipeline_sanity_checks.rds"), required = FALSE)

current_relevant <- run_row_count(relevant_history, current_run_id)
current_mitre <- run_row_count(mitre_history, current_run_id)
baseline_relevant <- run_row_count(relevant_history, previous_run_id)
baseline_mitre <- run_row_count(mitre_history, previous_run_id)

current_tool_keys <- run_unique_keys(relevant_history, current_run_id, c("record_id"))
baseline_tool_keys <- run_unique_keys(relevant_history, previous_run_id, c("record_id"))
current_mitre_keys <- run_unique_keys(mitre_history, current_run_id, c("record_id", "technique_id", "tactic"))
baseline_mitre_keys <- run_unique_keys(mitre_history, previous_run_id, c("record_id", "technique_id", "tactic"))

new_tools <- setdiff(current_tool_keys, baseline_tool_keys)
removed_tools <- setdiff(baseline_tool_keys, current_tool_keys)
new_mitre <- setdiff(current_mitre_keys, baseline_mitre_keys)
removed_mitre <- setdiff(baseline_mitre_keys, current_mitre_keys)

cat("\n=== Verification Summary ===\n")
cat(sprintf("Current run_id: %s\n", ifelse(is.na(current_run_id), "n/a", current_run_id)))
cat(sprintf("Baseline run_id: %s\n", ifelse(is.na(previous_run_id), "none", previous_run_id)))
cat(sprintf("Relevant tools: %s (baseline %s, delta %+d)\n", current_relevant, baseline_relevant, current_relevant - baseline_relevant))
cat(sprintf("MITRE mappings: %s (baseline %s, delta %+d)\n", current_mitre, baseline_mitre, current_mitre - baseline_mitre))
cat(sprintf("Tool diff: new %s | removed %s\n", length(new_tools), length(removed_tools)))
cat(sprintf("MITRE diff: new %s | removed %s\n", length(new_mitre), length(removed_mitre)))

if (is.data.frame(sanity_checks) && nrow(sanity_checks) > 0) {
  sanity_checks <- sanity_checks[order(sanity_checks[["status"]], sanity_checks[["severity"]], sanity_checks[["check_name"]]), , drop = FALSE]
  cat("\n=== Sanity Checks ===\n")
  print(sanity_checks[, c("check_name", "status", "severity", "observed_value", "expected_value")])
}

cat("\n=== Pipeline Status ===\n")
print(status_table)
log_message("Verification run finished")
