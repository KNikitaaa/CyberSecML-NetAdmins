project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
app_dir <- file.path(project_root, "inst", "shiny")

if (!dir.exists(app_dir)) {
  stop(sprintf("Shiny app directory not found: %s", app_dir), call. = FALSE)
}

shiny::runApp(app_dir, launch.browser = TRUE)