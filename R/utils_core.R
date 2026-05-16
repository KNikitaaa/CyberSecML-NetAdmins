#' Log a pipeline message
#'
#' Writes a formatted message to the console for pipeline tracing.
#'
#' @param message Message text.
#' @param level Message level such as `INFO`, `WARN`, or `ERROR`.
#'
#' @return Invisibly returns the formatted message.
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted <- sprintf("[%s] [%s] %s", timestamp, level, message)
  message(formatted)
  invisible(formatted)
}

#' Resolve the default pipeline data directory
#'
#' Uses an option or environment variable when available, otherwise falls back
#' to the project `inst/extdata` directory.
#'
#' @return Character scalar with a directory path.
get_default_data_dir <- function() {
  option_dir <- getOption("offensivetoolmapper.data_dir")
  env_dir <- Sys.getenv("OFFENSIVETOOLMAPPER_DATA_DIR", unset = "")
  package_dir <- system.file("extdata", package = "OffensiveToolMapper")
  source_dir <- file.path(getwd(), "inst", "extdata")

  candidates <- c(option_dir, env_dir, source_dir, package_dir)
  candidates <- candidates[nzchar(candidates)]

  if (length(candidates) == 0) {
    return(source_dir)
  }

  candidates[[1]]
}

#' Ensure a directory exists
#'
#' @param path Directory path.
#'
#' @return Invisibly returns the normalized path.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Save an object as pipeline RDS
#'
#' @param object R object to serialize.
#' @param file_path Output file path.
#'
#' @return Invisibly returns the file path.
save_pipeline_rds <- function(object, file_path) {
  ensure_dir(dirname(file_path))
  saveRDS(object, file = file_path)
  invisible(file_path)
}

#' Load an object from pipeline RDS
#'
#' @param file_path Input file path.
#' @param required Whether to error when the file is absent.
#'
#' @return Loaded R object or `NULL` when missing and `required` is `FALSE`.
load_pipeline_rds <- function(file_path, required = TRUE) {
  if (!file.exists(file_path)) {
    if (isTRUE(required)) {
      stop(sprintf("Required file not found: %s", file_path), call. = FALSE)
    }

    return(NULL)
  }

  readRDS(file_path)
}

#' Read a required environment variable
#'
#' @param name Environment variable name.
#'
#' @return Character scalar with the environment variable value.
require_env_var <- function(name) {
  value <- Sys.getenv(name, unset = "")

  if (!nzchar(value)) {
    stop(sprintf("Environment variable '%s' is required.", name), call. = FALSE)
  }

  value
}

#' Safely execute an expression
#'
#' @param expr Callable function with no arguments.
#' @param error_context Human-readable context for the error.
#'
#' @return Result of `expr()`.
safe_run <- function(expr, error_context = "Operation failed") {
  tryCatch(
    expr(),
    error = function(error) {
      stop(sprintf("%s: %s", error_context, conditionMessage(error)), call. = FALSE)
    }
  )
}

#' Perform a JSON HTTP request safely
#'
#' @param request An `httr2` request object.
#' @param simplify_data_frame Whether to simplify JSON objects to data frames.
#' @param error_context Human-readable context for the error.
#'
#' @return Parsed JSON response.
safe_json_request <- function(
  request,
  simplify_data_frame = TRUE,
  error_context = "HTTP request failed"
) {
  safe_run(
    expr = function() {
      response <- httr2::req_perform(request)
      httr2::resp_check_status(response)

      jsonlite::fromJSON(
        txt = httr2::resp_body_string(response),
        simplifyDataFrame = simplify_data_frame
      )
    },
    error_context = error_context
  )
}

#' Read a variable from .env when process env is empty
#'
#' @param name Variable name.
#' @param path Path to the dotenv file.
#'
#' @return Character scalar, or empty string when not found.
read_dotenv_value <- function(name, path = getOption("offensivetoolmapper.dotenv_path", ".env")) {
  if (!file.exists(path)) {
    return("")
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- stringr::str_trim(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!startsWith(lines, "#")]

  for (line in lines) {
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      next
    }

    key <- stringr::str_trim(parts[[1]])
    if (!identical(key, name)) {
      next
    }

    value <- paste(parts[-1], collapse = "=")
    value <- stringr::str_trim(value)
    value <- gsub('^"|"$', "", value)
    value <- gsub("^'|'$", "", value)
    return(value)
  }

  ""
}

#' Read runtime configuration from process env or .env fallback
#'
#' @param name Variable name.
#' @param unset Default value when missing.
#'
#' @return Character scalar.
get_runtime_env_value <- function(name, unset = "") {
  value <- Sys.getenv(name, unset = NA_character_)
  if (!is.na(value) && nzchar(value)) {
    return(value)
  }

  dotenv_value <- read_dotenv_value(name)
  if (nzchar(dotenv_value)) {
    return(dotenv_value)
  }

  unset
}
