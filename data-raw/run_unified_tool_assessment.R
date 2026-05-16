project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "llm_validation.R"))
source(file.path(project_root, "R", "storage_duckdb.R"))
source(file.path(project_root, "R", "llm_assessment.R"))
source(file.path(project_root, "R", "visualization_data.R"))

log_message("Running unified one-call tool assessment stage")

provider <- get_default_llm_provider()
runtime_config <- get_llm_runtime_config(provider)
max_records <- get_default_llm_max_records()

log_message(sprintf(
  "Assessment preflight: provider=%s model=%s base_url=%s api_key_present=%s max_records=%s",
  runtime_config$provider[[1]],
  runtime_config$model[[1]],
  runtime_config$base_url[[1]],
  runtime_config$api_key_present[[1]],
  ifelse(is.null(max_records), "all", as.character(max_records))
))

result <- run_unified_tool_assessment(
  normalized_data = load_pipeline_rds(file.path(project_root, "inst", "extdata", "normalized_tools.rds")),
  output_path = file.path(project_root, "inst", "extdata", "tool_assessment_results.rds"),
  relevant_output_path = file.path(project_root, "inst", "extdata", "relevant_tools.rds"),
  mitre_output_path = file.path(project_root, "inst", "extdata", "tool_mitre_mappings.rds"),
  duckdb_path = file.path(project_root, "inst", "extdata", "offensive_tool_mapper.duckdb"),
  provider = provider,
  model = runtime_config$model[[1]],
  base_url = runtime_config$base_url[[1]],
  max_records = max_records,
  api_key = get_llm_api_key(provider)
)

visualization <- build_visualization_dataset(
  assessment_results = result$assessment_results,
  mitre_mappings = result$mitre_mappings,
  output_path = file.path(project_root, "inst", "extdata", "visualization_tools.rds"),
  matrix_output_path = file.path(project_root, "inst", "extdata", "visualization_tool_matrix.rds"),
  duckdb_path = file.path(project_root, "inst", "extdata", "offensive_tool_mapper.duckdb"),
  write_duckdb = TRUE
)

summary <- tibble::tibble(
  dataset = c("assessment_results", "relevant_tools", "mitre_mappings", "llm_processing_queue", "visualization_tools", "visualization_tool_matrix"),
  rows = c(nrow(result$assessment_results), nrow(result$relevant_tools), nrow(result$mitre_mappings), nrow(result$llm_processing_queue), nrow(visualization$visualization_tools), nrow(visualization$visualization_tool_matrix))
)

print(summary)
log_message("Unified one-call tool assessment stage finished")