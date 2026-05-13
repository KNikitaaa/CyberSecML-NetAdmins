`%||%` <- function(left, right) {
  if (is.null(left) || !nzchar(left)) {
    return(right)
  }

  left
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  ""
}

project_root <- normalizePath(
  file.path(dirname(script_path %||% getwd()), "..", ".."),
  winslash = "/",
  mustWork = FALSE
)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(project_root, quiet = TRUE, export_all = FALSE)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(project_root, quiet = TRUE, export_all = FALSE)
} else {
  library(OffensiveToolMapper)
}

transport <- Sys.getenv("OTM_MCP_TRANSPORT", unset = "stdio")
port <- suppressWarnings(as.integer(Sys.getenv("OTM_MCP_PORT", unset = "3000")))
if (is.na(port) || port <= 0) {
  port <- 3000L
}

data_dir <- Sys.getenv(
  "OFFENSIVETOOLMAPPER_DATA_DIR",
  unset = file.path(project_root, "inst", "extdata")
)

OffensiveToolMapper::run_mcp_server(
  transport = transport,
  data_dir = data_dir,
  port = port
)