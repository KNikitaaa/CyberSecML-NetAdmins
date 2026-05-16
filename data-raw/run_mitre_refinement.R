project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "llm_validation.R"))
source(file.path(project_root, "R", "llm_assessment.R"))
source(file.path(project_root, "R", "rag_refinement.R"))

log_message("Running MITRE retrieval refinement stage")

result <- run_mitre_refinement(
  assessment_results = load_pipeline_rds(file.path(project_root, "inst", "extdata", "tool_assessment_results.rds"), required = TRUE),
  mitre_mappings = load_pipeline_rds(file.path(project_root, "inst", "extdata", "tool_mitre_mappings.rds"), required = FALSE),
  data_dir = file.path(project_root, "inst", "extdata"),
  mitre_attack_path = file.path(project_root, "data", "mitre_attack.rda")
)

print(utils::head(result, 20))
log_message("MITRE retrieval refinement stage finished")