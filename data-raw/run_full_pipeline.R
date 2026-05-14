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

provider <- get_default_llm_provider()
runtime_config <- get_llm_runtime_config(provider)
max_records <- get_default_llm_max_records()
collect_mode <- match.arg(
  Sys.getenv("OTM_COLLECT_MODE", unset = "incremental"),
  choices = c("incremental", "snapshot")
)
github_min_stars <- as.integer(Sys.getenv("OTM_GITHUB_MIN_STARS", unset = "0"))
github_max_results <- as.integer(Sys.getenv("OTM_GITHUB_MAX_RESULTS", unset = "100"))
github_max_search_requests <- as.integer(Sys.getenv("OTM_GITHUB_MAX_SEARCH_REQUESTS", unset = "60"))

log_message(sprintf(
  "Full pipeline preflight: provider=%s model=%s base_url=%s api_key_present=%s max_records=%s collect_mode=%s github_min_stars=%s github_max_results=%s github_max_search_requests=%s",
  runtime_config$provider[[1]],
  runtime_config$model[[1]],
  runtime_config$base_url[[1]],
  runtime_config$api_key_present[[1]],
  ifelse(is.null(max_records), "all", as.character(max_records)),
  collect_mode,
  github_min_stars,
  github_max_results,
  github_max_search_requests
))

result <- run_full_pipeline(
  data_dir = file.path(project_root, "inst", "extdata"),
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

print(result$pipeline_status)
log_message("Full pipeline finished")
