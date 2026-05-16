## Run only the collect stage (GitHub + RSS + PacketStorm).
## Does NOT run LLM assessment — useful for testing data sources.
##
## Usage:
##   Rscript data-raw/run_collect_only.R
##
## Env vars that affect this script:
##   OTM_COLLECT_MODE          incremental (default) | snapshot
##   GITHUB_PAT                GitHub token (recommended)
##   OTM_GITHUB_MIN_STARS      default 10
##   OTM_GITHUB_MAX_RESULTS    default 100
##   OTM_GITHUB_MAX_SEARCH_REQUESTS  default 60
##   OTM_RSS_FEEDS             leave empty = 3 default feeds, or semicolon-separated URLs, or "disabled"
##   PACKETSTORM_API_SECRET    PacketStorm API key (optional)
##   PACKETSTORM_URLS          semicolon-separated PacketStorm page URLs (optional, no key needed)

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

collect_mode <- match.arg(
  get_runtime_env_value("OTM_COLLECT_MODE", unset = "incremental"),
  choices = c("incremental", "snapshot")
)
github_min_stars          <- as.integer(get_runtime_env_value("OTM_GITHUB_MIN_STARS",          unset = "10"))
github_max_results        <- as.integer(get_runtime_env_value("OTM_GITHUB_MAX_RESULTS",        unset = "100"))
github_max_search_requests <- as.integer(get_runtime_env_value("OTM_GITHUB_MAX_SEARCH_REQUESTS", unset = "60"))

rss_feeds <- get_default_feeds()
rss_label <- if (length(rss_feeds) == 0) "disabled" else paste(length(rss_feeds), "feeds")

packetstorm_categories <- get_default_packetstorm_categories()
packetstorm_urls       <- get_default_packetstorm_manual_urls()
ps_label <- if (length(packetstorm_categories) == 0 && length(packetstorm_urls) == 0) {
  "disabled"
} else {
  paste0(length(packetstorm_categories), " categories + ", length(packetstorm_urls), " manual URLs")
}

log_message(sprintf(
  "Collect-only run: collect_mode=%s github_min_stars=%s github_max_search_requests=%s rss=%s packetstorm=%s",
  collect_mode, github_min_stars, github_max_search_requests, rss_label, ps_label
))

result <- run_pipeline_from(
  stage      = "collect",
  end_stage  = "normalize",
  data_dir   = file.path(project_root, "inst", "extdata"),
  collect_mode              = collect_mode,
  github_min_stars          = github_min_stars,
  github_max_results        = github_max_results,
  github_max_search_requests = github_max_search_requests,
  process_existing_backlog  = FALSE,
  write_duckdb              = TRUE,
  mitre_attack_path         = file.path(project_root, "data", "mitre_attack.rda")
)

raw <- result$raw_inputs
cat("\n--- Collect results ---\n")
cat(sprintf("GitHub:       %d rows\n", if (!is.null(raw$github))       nrow(raw$github)       else 0L))
cat(sprintf("RSS:          %d rows\n", if (!is.null(raw$rss))          nrow(raw$rss)          else 0L))
cat(sprintf("PacketStorm:  %d rows\n", if (!is.null(raw$packetstorm))  nrow(raw$packetstorm)  else 0L))
if (!is.null(result$normalized_data)) {
  cat(sprintf("Normalized:   %d candidates total\n", nrow(result$normalized_data)))
}
cat("-----------------------\n")
log_message("Collect-only run finished")
