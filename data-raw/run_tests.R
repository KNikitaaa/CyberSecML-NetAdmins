project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run the local test suite.", call. = FALSE)
}

setwd(project_root)

testthat::test_local(
  path = project_root,
  load_package = "source",
  reporter = "summary",
  stop_on_failure = TRUE
)
