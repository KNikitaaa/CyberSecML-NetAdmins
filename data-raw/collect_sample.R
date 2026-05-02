project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

source(file.path(project_root, "R", "utils_core.R"))
source(file.path(project_root, "R", "text_processing.R"))
source(file.path(project_root, "R", "etl_github.R"))
source(file.path(project_root, "R", "etl_packetstorm.R"))
source(file.path(project_root, "R", "etl_rss.R"))

log_message("Collecting raw sample dataset")

github_raw <- collect_github_tools(
  max_results = 25,
  min_stars = 50L,
  output_path = file.path(project_root, "inst", "extdata", "raw_github.rds")
)

packetstorm_raw <- scrape_packetstorm(
  categories = get_default_packetstorm_categories(),
  include_linked_content = TRUE,
  output_path = file.path(project_root, "inst", "extdata", "raw_packetstorm.rds")
)

rss_raw <- fetch_security_feeds(
  include_linked_content = TRUE,
  output_path = file.path(project_root, "inst", "extdata", "raw_rss.rds")
)

summary <- tibble::tibble(
  source = c("github", "packetstorm", "rss"),
  rows = c(nrow(github_raw), nrow(packetstorm_raw), nrow(rss_raw))
)

print(summary)
log_message("Raw sample dataset collection finished")