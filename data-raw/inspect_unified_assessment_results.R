project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

assessment_path <- file.path(project_root, "inst", "extdata", "tool_assessment_results.rds")

if (!file.exists(assessment_path)) {
  stop(sprintf("Assessment results file not found: %s", assessment_path), call. = FALSE)
}

results <- readRDS(assessment_path)

cat("assessment_rows=", nrow(results), "\n", sep = "")
cat("\nstatus_counts\n")
print(as.data.frame(table(results$llm_status, useNA = "ifany")))

cat("\nruntime_config\n")
runtime_columns <- intersect(c("llm_provider", "llm_model", "llm_base_url"), names(results))
if (length(runtime_columns) > 0) {
  print(unique(results[, runtime_columns, drop = FALSE]))
}

cat("\nrelevance_counts\n")
if ("is_relevant" %in% names(results)) {
  print(as.data.frame(table(results$is_relevant, useNA = "ifany")))
}

cat("\nentity_type_counts\n")
if ("entity_type" %in% names(results)) {
  print(utils::head(sort(table(results$entity_type, useNA = "ifany"), decreasing = TRUE), 20))
}

cat("\nfirst_errors\n")
errors <- unique(stats::na.omit(results$llm_error))
if (length(errors) == 0) {
  cat("No non-NA llm_error values.\n")
} else {
  print(utils::head(errors, 10))
}

cat("\nfirst_rows\n")
preview_columns <- intersect(
  c("record_id", "name", "source", "pre_llm_should_process", "llm_status", "is_relevant", "entity_type", "assessed_name", "llm_error"),
  names(results)
)
print(utils::head(results[, preview_columns, drop = FALSE], 20))