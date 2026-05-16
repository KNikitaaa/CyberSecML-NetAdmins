args <- commandArgs(trailingOnly = TRUE)
all_args <- commandArgs(trailingOnly = FALSE)

file_arg <- "--file="
script_arg <- all_args[startsWith(all_args, file_arg)]

script_path <- if (length(script_arg) > 0) {
  normalizePath(sub(file_arg, "", script_arg[[1]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- if (dir.exists(script_path)) {
  script_path
} else {
  dirname(script_path)
}

project_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)

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

find_arg_value <- function(args, name) {
  matches <- args[startsWith(args, paste0("--", name, "="))]
  if (length(matches) == 0) {
    return(NULL)
  }

  sub(paste0("^--", name, "="), "", matches[[1]])
}

parse_run_count <- function(args) {
  run_value <- find_arg_value(args, "runs")

  if (is.null(run_value)) {
    positional <- args[!startsWith(args, "--")]
    run_value <- if (length(positional) > 0) positional[[1]] else "5"
  }

  run_count <- suppressWarnings(as.integer(run_value))

  if (is.na(run_count) || run_count < 1L) {
    stop("Batch run count must be a positive integer.", call. = FALSE)
  }

  as.integer(run_count)
}

scalar_or <- function(value, default = NA) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  scalar <- value[[1]]
  if (length(scalar) == 0 || (length(scalar) == 1 && is.na(scalar))) {
    return(default)
  }

  scalar
}

empty_pipeline_batch_summary <- function() {
  tibble::tibble(
    run_index = integer(),
    batch_id = character(),
    batch_iteration = integer(),
    requested_runs = integer(),
    status = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    github_new_rows = integer(),
    github_rows_total = integer(),
    github_selected_requests = integer(),
    github_logged_requests = integer(),
    github_request_offset = integer(),
    github_next_request_offset = integer(),
    github_rotation_progress_percent = numeric(),
    github_remaining_runs_estimate = integer(),
    github_query_families = character(),
    github_query_tiers = character(),
    error_message = character()
  )
}

load_batch_summary <- function(path) {
  summary <- load_pipeline_rds(path, required = FALSE)

  if (!is.data.frame(summary)) {
    return(empty_pipeline_batch_summary())
  }

  dplyr::bind_rows(empty_pipeline_batch_summary(), tibble::as_tibble(summary))
}

save_batch_summary <- function(batch_summary, path) {
  save_pipeline_rds(batch_summary, path)

  json_path <- sub("[.]rds$", ".json", path)
  if (!identical(json_path, path)) {
    jsonlite::write_json(batch_summary, json_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  }

  invisible(path)
}

parse_details_json <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(list())
  }

  json_text <- as.character(value[[1]])
  if (!nzchar(json_text)) {
    return(list())
  }

  tryCatch(
    jsonlite::fromJSON(json_text, simplifyVector = TRUE),
    error = function(error) {
      log_message(sprintf("Unable to parse pipeline details JSON: %s", conditionMessage(error)), level = "WARN")
      list()
    }
  )
}

extract_collect_details <- function(status_table) {
  if (!is.data.frame(status_table) || nrow(status_table) == 0) {
    return(list())
  }

  collect_rows <- status_table[status_table[["stage"]] == "collect", , drop = FALSE]
  if (nrow(collect_rows) == 0) {
    return(list())
  }

  parse_details_json(utils::tail(collect_rows[["details_json"]], 1))
}

next_batch_run_index <- function(batch_summary) {
  if (!is.data.frame(batch_summary) || nrow(batch_summary) == 0) {
    return(1L)
  }

  run_indexes <- suppressWarnings(as.integer(batch_summary[["run_index"]]))
  run_indexes <- run_indexes[!is.na(run_indexes)]

  if (length(run_indexes) == 0) {
    return(1L)
  }

  as.integer(max(run_indexes) + 1L)
}

build_batch_summary_row <- function(run_index, batch_id, batch_iteration, requested_runs, started_at, finished_at, status_table, error_message = NA_character_) {
  collect_details <- extract_collect_details(status_table)
  run_status <- if (is.data.frame(status_table) && nrow(status_table) > 0 && any(status_table[["status"]] == "error")) {
    "error"
  } else {
    "success"
  }

  tibble::tibble(
    run_index = as.integer(run_index),
    batch_id = batch_id,
    batch_iteration = as.integer(batch_iteration),
    requested_runs = as.integer(requested_runs),
    status = run_status,
    started_at = format(started_at, "%Y-%m-%d %H:%M:%S"),
    finished_at = format(finished_at, "%Y-%m-%d %H:%M:%S"),
    duration_seconds = round(as.numeric(difftime(finished_at, started_at, units = "secs")), 3),
    github_new_rows = as.integer(scalar_or(collect_details$github_new_rows, NA_integer_)),
    github_rows_total = as.integer(scalar_or(collect_details$github_rows, NA_integer_)),
    github_selected_requests = as.integer(scalar_or(collect_details$github_selected_requests, NA_integer_)),
    github_logged_requests = as.integer(scalar_or(collect_details$github_logged_requests, NA_integer_)),
    github_request_offset = as.integer(scalar_or(collect_details$github_request_offset, NA_integer_)),
    github_next_request_offset = as.integer(scalar_or(collect_details$github_next_request_offset, NA_integer_)),
    github_rotation_progress_percent = as.numeric(scalar_or(collect_details$github_rotation_progress_percent, NA_real_)),
    github_remaining_runs_estimate = as.integer(scalar_or(collect_details$github_remaining_runs_estimate, NA_integer_)),
    github_query_families = as.character(scalar_or(collect_details$github_logged_query_families, NA_character_)),
    github_query_tiers = as.character(scalar_or(collect_details$github_logged_query_tiers, NA_character_)),
    error_message = as.character(error_message)
  )
}

run_count <- parse_run_count(args)
continue_on_error <- "--continue-on-error" %in% args

provider <- get_default_llm_provider()
runtime_config <- get_llm_runtime_config(provider)
max_records <- get_default_llm_max_records()
collect_mode <- match.arg(
  get_runtime_env_value("OTM_COLLECT_MODE", unset = "incremental"),
  choices = c("incremental", "snapshot")
)
github_min_stars <- as.integer(get_runtime_env_value("OTM_GITHUB_MIN_STARS", unset = "10"))
github_max_results <- as.integer(get_runtime_env_value("OTM_GITHUB_MAX_RESULTS", unset = "100"))
github_max_search_requests <- as.integer(get_runtime_env_value("OTM_GITHUB_MAX_SEARCH_REQUESTS", unset = "60"))

data_dir <- file.path(project_root, "inst", "extdata")
status_path <- file.path(data_dir, "pipeline_status.rds")
batch_summary_path <- .pipeline_batch_summary_path(data_dir)
batch_summary <- load_batch_summary(batch_summary_path)
batch_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
run_index <- next_batch_run_index(batch_summary)

log_message(sprintf(
  "Batch pipeline preflight: runs=%s continue_on_error=%s provider=%s model=%s collect_mode=%s github_min_stars=%s github_max_results=%s github_max_search_requests=%s",
  run_count,
  continue_on_error,
  runtime_config$provider[[1]],
  runtime_config$model[[1]],
  collect_mode,
  github_min_stars,
  github_max_results,
  github_max_search_requests
))

setwd(project_root)

for (batch_iteration in seq_len(run_count)) {
  started_at <- Sys.time()
  log_message(sprintf("Batch pipeline run %s/%s started", batch_iteration, run_count))

  status_table <- NULL
  run_error <- NA_character_

  tryCatch(
    {
      result <- run_full_pipeline(
        data_dir = data_dir,
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

      status_table <- result$pipeline_status
    },
    error = function(error) {
      run_error <<- conditionMessage(error)
      status_table <<- load_pipeline_rds(status_path, required = FALSE)
      log_message(sprintf("Batch pipeline run %s/%s failed: %s", batch_iteration, run_count, run_error), level = "ERROR")
    }
  )

  finished_at <- Sys.time()
  summary_row <- build_batch_summary_row(
    run_index = run_index,
    batch_id = batch_id,
    batch_iteration = batch_iteration,
    requested_runs = run_count,
    started_at = started_at,
    finished_at = finished_at,
    status_table = status_table,
    error_message = run_error
  )

  batch_summary <- dplyr::bind_rows(batch_summary, summary_row)
  save_batch_summary(batch_summary, batch_summary_path)

  log_message(sprintf(
    "Batch pipeline run %s/%s finished: status=%s duration=%ss github_new_rows=%s next_offset=%s",
    batch_iteration,
    run_count,
    summary_row$status[[1]],
    summary_row$duration_seconds[[1]],
    summary_row$github_new_rows[[1]],
    summary_row$github_next_request_offset[[1]]
  ))

  if (!is.na(run_error) && !isTRUE(continue_on_error)) {
    stop(sprintf("Batch pipeline aborted on run %s/%s: %s", batch_iteration, run_count, run_error), call. = FALSE)
  }

  run_index <- run_index + 1L
}

current_batch <- batch_summary[batch_summary[["batch_id"]] == batch_id, , drop = FALSE]
print(current_batch)
log_message(sprintf("Batch pipeline finished; summary saved to %s", batch_summary_path))
