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
build_pkgdown <- "--pkgdown" %in% args

find_pandoc_dir <- function() {
  existing <- Sys.getenv("RSTUDIO_PANDOC", unset = "")
  if (nzchar(existing) && file.exists(file.path(existing, "pandoc.exe"))) {
    return(normalizePath(existing, winslash = "/", mustWork = TRUE))
  }

  candidates <- c(
    "C:/Program Files/Pandoc",
    "C:/Program Files/RStudio/bin/pandoc",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    file.path(Sys.getenv("LOCALAPPDATA", unset = ""), "Programs", "Quarto", "bin", "tools"),
    file.path(Sys.getenv("LOCALAPPDATA", unset = ""), "Pandoc")
  )

  for (candidate in candidates[nzchar(candidates)]) {
    if (file.exists(file.path(candidate, "pandoc.exe"))) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  NULL
}

if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop("Package 'roxygen2' is required to generate Rd files and NAMESPACE.", call. = FALSE)
}

setwd(project_root)

message("Generating roxygen2 reference artifacts in: ", project_root)

roxygen2::roxygenise(
  package.dir = project_root,
  roclets = c("namespace", "rd"),
  load = "source"
)

rd_count <- length(list.files(file.path(project_root, "man"), pattern = "\\.Rd$"))
message("Reference artifacts updated. Rd files present: ", rd_count)

if (!build_pkgdown) {
  message("pkgdown build not requested. Use '--pkgdown' to build the site when pkgdown is available.")
} else if (!requireNamespace("pkgdown", quietly = TRUE)) {
  stop("Package 'pkgdown' is not installed. Install it or rerun without '--pkgdown'.", call. = FALSE)
} else {
  pandoc_dir <- find_pandoc_dir()
  if (is.null(pandoc_dir)) {
    stop(
      paste(
        "Pandoc was not found.",
        "Install Pandoc or RStudio/Quarto, or set RSTUDIO_PANDOC to a directory containing pandoc.exe."
      ),
      call. = FALSE
    )
  }

  Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
  message("Using Pandoc from: ", pandoc_dir)
  message("Building pkgdown site...")
  pkgdown::build_site(pkg = project_root, install = TRUE, new_process = FALSE, preview = FALSE)
  message("pkgdown site build finished.")
}