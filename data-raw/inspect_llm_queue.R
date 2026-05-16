project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "llm_validation.R"))
source(file.path(project_root, "R", "storage_duckdb.R"))
source(file.path(project_root, "R", "llm_assessment.R"))

data_dir <- file.path(project_root, "inst", "extdata")

queue <- build_llm_processing_queue(
  normalized_data = load_pipeline_rds(file.path(data_dir, "normalized_tools.rds"), required = TRUE),
  assessment_results = load_pipeline_rds(file.path(data_dir, "tool_assessment_results.rds"), required = FALSE),
  data_dir = data_dir,
  output_path = file.path(data_dir, "llm_processing_queue.rds"),
  duckdb_path = file.path(data_dir, "offensive_tool_mapper.duckdb"),
  write_duckdb = TRUE
)

status_summary <- queue |>
  dplyr::count(queue_status, sort = TRUE, name = "rows")

pending_summary <- queue |>
  dplyr::filter(queue_status %in% c("pending_not_sent", "retry_after_error")) |>
  dplyr::select(name, source, pre_llm_score, date_found, queue_status, llm_error) |>
  dplyr::slice_head(n = 25)

processed_summary <- queue |>
  dplyr::filter(queue_status == "processed_success") |>
  dplyr::select(name, source, pre_llm_score, llm_processed_at, assessed_name, overall_confidence) |>
  dplyr::slice_head(n = 25)

cat("LLM queue status summary\n")
print(status_summary)
cat("\nPending or retry rows\n")
print(pending_summary)
cat("\nProcessed rows\n")
print(processed_summary)