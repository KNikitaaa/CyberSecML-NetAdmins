project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "normalize.R"))
source(file.path(project_root, "R", "llm_provider.R"))
source(file.path(project_root, "R", "llm_contracts.R"))
source(file.path(project_root, "R", "llm_validation.R"))

log_message("Running LLM validation/enrichment stage")

provider <- get_default_llm_provider()
runtime_config <- get_llm_runtime_config(provider)

log_message(sprintf(
  "Validation preflight: provider=%s model=%s base_url=%s api_key_present=%s",
  runtime_config$provider[[1]],
  runtime_config$model[[1]],
  runtime_config$base_url[[1]],
  runtime_config$api_key_present[[1]]
))

result <- run_validation_enrichment(
  normalized_data = load_pipeline_rds(file.path(project_root, "inst", "extdata", "normalized_tools.rds")),
  output_path = file.path(project_root, "inst", "extdata", "validation_enrichment_results.rds"),
  enriched_output_path = file.path(project_root, "inst", "extdata", "enriched_tools.rds"),
  provider = provider,
  model = runtime_config$model[[1]],
  base_url = runtime_config$base_url[[1]],
  api_key = get_llm_api_key(provider)
)

summary <- tibble::tibble(
  dataset = c("validation_results", "enriched_tools"),
  rows = c(nrow(result$validation_results), nrow(result$enriched_tools))
)

print(summary)
log_message("LLM validation/enrichment stage finished")