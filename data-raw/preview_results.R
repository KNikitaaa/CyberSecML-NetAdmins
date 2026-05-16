project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
extdata_dir <- file.path(project_root, "inst", "extdata")

.print_section <- function(title) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n", sep = "")
  cat(title, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n", sep = "")
}

.load_rds_if_exists <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  readRDS(path)
}

.preview_table <- function(data, columns, max_rows = 10L) {
  if (is.null(data)) {
    cat("File not found.\n")
    return(invisible(NULL))
  }

  cat("rows=", nrow(data), "\n", sep = "")
  preview_columns <- intersect(columns, names(data))

  if (length(preview_columns) == 0) {
    cat("No preview columns found.\n")
    return(invisible(NULL))
  }

  print(utils::head(data[, preview_columns, drop = FALSE], max_rows))
  invisible(NULL)
}

assessment_results <- .load_rds_if_exists(file.path(extdata_dir, "tool_assessment_results.rds"))
relevant_tools <- .load_rds_if_exists(file.path(extdata_dir, "relevant_tools.rds"))
mitre_mappings <- .load_rds_if_exists(file.path(extdata_dir, "tool_mitre_mappings.rds"))
visualization_tools <- .load_rds_if_exists(file.path(extdata_dir, "visualization_tools.rds"))
visualization_matrix <- .load_rds_if_exists(file.path(extdata_dir, "visualization_tool_matrix.rds"))
duckdb_path <- file.path(extdata_dir, "offensive_tool_mapper.duckdb")

.print_section("Assessment Results")
.preview_table(
  assessment_results,
  c("record_id", "name", "source", "llm_status", "is_relevant", "entity_type", "assessed_name", "llm_provider", "llm_model")
)

if (!is.null(assessment_results) && "llm_status" %in% names(assessment_results)) {
  cat("\nstatus_counts\n")
  print(as.data.frame(table(assessment_results$llm_status, useNA = "ifany")))
}

.print_section("Relevant Tools")
.preview_table(
  relevant_tools,
  c("assessed_name", "source", "entity_type", "category_ru", "summary_ru", "overall_confidence")
)

.print_section("MITRE Mappings")
.preview_table(
  mitre_mappings,
  c("assessed_name", "technique_id", "technique_name", "tactic", "confidence")
)

.print_section("Visualization Tools")
.preview_table(
  visualization_tools,
  c("assessed_name", "source", "entity_type", "short_description_ru", "long_description_ru", "mitre_tactics", "mitre_technique_ids", "filter_tags", "pre_llm_score", "confidence_score")
)

.print_section("Visualization Matrix")
.preview_table(
  visualization_matrix,
  c("assessed_name", "technique_id", "technique_name", "tactic", "tactic_tag", "technique_tag")
)

.print_section("DuckDB")
if (!file.exists(duckdb_path)) {
  cat("DuckDB file not found.\n")
} else {
  cat("path=", duckdb_path, "\n", sep = "")

  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE)) {
    cat("DBI or duckdb package is not available in this environment.\n")
  } else {
    connection <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

    tables <- DBI::dbListTables(connection)
    if (length(tables) == 0) {
      cat("No tables found.\n")
    } else {
      table_counts <- lapply(
        tables,
        function(table_name) {
          count_value <- DBI::dbGetQuery(connection, sprintf("SELECT COUNT(*) AS n FROM %s", table_name))$n[[1]]
          data.frame(table_name = table_name, rows = count_value)
        }
      )

      print(do.call(rbind, table_counts))
    }
  }
}