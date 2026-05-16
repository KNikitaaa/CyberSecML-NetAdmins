project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

targets <- c(
  file.path(project_root, "inst", "extdata", "debug_deepseek_unified_response.txt"),
  file.path(project_root, "inst", "extdata", "github_search_state.rds"),
  file.path(project_root, "inst", "extdata", "mitre_refinement_candidates.rds"),
  file.path(project_root, "inst", "extdata", "mitre_refinement_index.rds"),
  file.path(project_root, "inst", "extdata", "normalized_tools.rds"),
  file.path(project_root, "inst", "extdata", "offensive_tool_mapper.duckdb"),
  file.path(project_root, "inst", "extdata", "pipeline_status.json"),
  file.path(project_root, "inst", "extdata", "pipeline_status.rds"),
  file.path(project_root, "inst", "extdata", "raw_github.rds"),
  file.path(project_root, "inst", "extdata", "raw_packetstorm.rds"),
  file.path(project_root, "inst", "extdata", "raw_rss.rds"),
  file.path(project_root, "inst", "extdata", "relevant_tools.rds"),
  file.path(project_root, "inst", "extdata", "llm_processing_queue.rds"),
  file.path(project_root, "inst", "extdata", "tool_assessment_results.rds"),
  file.path(project_root, "inst", "extdata", "tool_mitre_mappings.rds"),
  file.path(project_root, "inst", "extdata", "visualization_tools.rds"),
  file.path(project_root, "inst", "extdata", "visualization_tool_history.rds"),
  file.path(project_root, "inst", "extdata", "visualization_tool_matrix.rds")
)

existing_targets <- targets[file.exists(targets)]

if (length(existing_targets) == 0) {
  cat("No generated pipeline artifacts found to delete.\n")
} else {
  file.remove(existing_targets)
  cat(sprintf("Deleted %s generated artifacts.\n", length(existing_targets)))
  cat(paste(existing_targets, collapse = "\n"), sep = "")
  cat("\n")
}
