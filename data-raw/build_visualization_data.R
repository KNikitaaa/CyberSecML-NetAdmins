project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "storage_duckdb.R"))
source(file.path(project_root, "R", "visualization_data.R"))

log_message("Building visualization dataset")

result <- build_visualization_dataset(
  assessment_results = load_pipeline_rds(file.path(project_root, "inst", "extdata", "tool_assessment_results.rds"), required = TRUE),
  mitre_mappings = load_pipeline_rds(file.path(project_root, "inst", "extdata", "tool_mitre_mappings.rds"), required = TRUE),
  output_path = file.path(project_root, "inst", "extdata", "visualization_tools.rds"),
  matrix_output_path = file.path(project_root, "inst", "extdata", "visualization_tool_matrix.rds"),
  duckdb_path = file.path(project_root, "inst", "extdata", "offensive_tool_mapper.duckdb"),
  write_duckdb = TRUE
)

summary <- tibble::tibble(
  dataset = c("visualization_tools", "visualization_tool_matrix"),
  rows = c(nrow(result$visualization_tools), nrow(result$visualization_tool_matrix))
)

print(summary)
log_message("Visualization dataset build finished")