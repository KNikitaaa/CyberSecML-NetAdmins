test_that("Russian user-facing text files do not contain mojibake", {
  project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
  paths <- file.path(project_root, c(
    "inst/shiny/app.R",
    "inst/shiny/www/app.css",
    "R/llm_assessment.R",
    "R/visualization_data.R",
    "README.md",
    "tests/testthat/test-pipeline.R"
  ))

  mojibake_markers <- c(
    "РЎ",
    "Рџ",
    "Рњ",
    "Рљ",
    "Рќ",
    "РёР",
    "РµР",
    "СЃ",
    "С‚",
    "вЂ"
  )

  offenders <- character()
  for (path in paths[file.exists(paths)]) {
    text <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    matched <- mojibake_markers[vapply(mojibake_markers, grepl, logical(1), x = text, fixed = TRUE)]
    if (length(matched) > 0) {
      offenders <- c(offenders, sprintf("%s: %s", basename(path), paste(matched, collapse = ", ")))
    }
  }

  expect_equal(offenders, character())
})
