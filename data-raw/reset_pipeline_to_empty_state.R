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
source(file.path(project_root, "R", "pipeline.R"))

data_dir <- file.path(project_root, "inst", "extdata")

targets <- c(
  file.path(data_dir, "debug_deepseek_unified_response.txt"),
  file.path(data_dir, "github_search_state.rds"),
  file.path(data_dir, "llm_processing_queue.rds"),
  file.path(data_dir, "mitre_refinement_candidates.rds"),
  file.path(data_dir, "mitre_refinement_index.rds"),
  file.path(data_dir, "normalized_tools.rds"),
  file.path(data_dir, "offensive_tool_mapper.duckdb"),
  file.path(data_dir, "pipeline_status.json"),
  file.path(data_dir, "pipeline_status.rds"),
  file.path(data_dir, "raw_github.rds"),
  file.path(data_dir, "raw_packetstorm.rds"),
  file.path(data_dir, "raw_rss.rds"),
  file.path(data_dir, "relevant_tools.rds"),
  file.path(data_dir, "tool_assessment_results.rds"),
  file.path(data_dir, "tool_mitre_mappings.rds"),
  file.path(data_dir, "validation_enrichment_results.rds"),
  file.path(data_dir, "enriched_tools.rds"),
  file.path(data_dir, "visualization_tools.rds"),
  file.path(data_dir, "visualization_tool_history.rds"),
  file.path(data_dir, "visualization_tool_matrix.rds")
)

existing_targets <- targets[file.exists(targets)]

if (length(existing_targets) > 0) {
  file.remove(existing_targets)
}

ensure_dir(data_dir)

raw_github <- .github_empty_detail_results()
raw_packetstorm <- .packetstorm_empty_results()
raw_rss <- .rss_empty_results()
normalized <- .normalize_empty_results()
validation_results <- .validation_empty_results()
assessment_results <- .assessment_empty_results()
mitre_mappings <- .assessment_empty_mitre_results()

save_pipeline_rds(raw_github, file.path(data_dir, "raw_github.rds"))
save_pipeline_rds(raw_packetstorm, file.path(data_dir, "raw_packetstorm.rds"))
save_pipeline_rds(raw_rss, file.path(data_dir, "raw_rss.rds"))
save_pipeline_rds(normalized, file.path(data_dir, "normalized_tools.rds"))
save_pipeline_rds(validation_results, file.path(data_dir, "validation_enrichment_results.rds"))
save_pipeline_rds(validation_results, file.path(data_dir, "enriched_tools.rds"))
save_pipeline_rds(assessment_results, file.path(data_dir, "tool_assessment_results.rds"))
save_pipeline_rds(assessment_results, file.path(data_dir, "relevant_tools.rds"))
save_pipeline_rds(mitre_mappings, file.path(data_dir, "tool_mitre_mappings.rds"))
save_pipeline_rds(tibble::tibble(), file.path(data_dir, "mitre_refinement_candidates.rds"))

queue <- build_llm_processing_queue(
  normalized_data = normalized,
  assessment_results = assessment_results,
  data_dir = data_dir,
  output_path = file.path(data_dir, "llm_processing_queue.rds"),
  write_duckdb = FALSE
)

visualization <- build_visualization_dataset(
  assessment_results = assessment_results,
  mitre_mappings = mitre_mappings,
  data_dir = data_dir,
  output_path = file.path(data_dir, "visualization_tools.rds"),
  matrix_output_path = file.path(data_dir, "visualization_tool_matrix.rds"),
  history_output_path = file.path(data_dir, "visualization_tool_history.rds"),
  write_duckdb = FALSE
)

.pipeline_save_status(.pipeline_empty_status(), file.path(data_dir, "pipeline_status.rds"))

cat(sprintf("Deleted %s generated artifacts and recreated empty state.\n", length(existing_targets)))
cat(sprintf("raw_github=%s\n", nrow(raw_github)))
cat(sprintf("raw_packetstorm=%s\n", nrow(raw_packetstorm)))
cat(sprintf("raw_rss=%s\n", nrow(raw_rss)))
cat(sprintf("normalized=%s\n", nrow(normalized)))
cat(sprintf("assessment_results=%s\n", nrow(assessment_results)))
cat(sprintf("llm_processing_queue=%s\n", nrow(queue)))
cat(sprintf("visualization_tools=%s\n", nrow(visualization$visualization_tools)))
cat(sprintf("visualization_tool_matrix=%s\n", nrow(visualization$visualization_tool_matrix)))
