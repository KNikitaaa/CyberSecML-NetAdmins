test_that("write_duckdb_table persists list columns as JSON text", {
  db_path <- tempfile(fileext = ".duckdb")
  init_pipeline_duckdb(db_path)

  data <- tibble::tibble(
    id = c("a", "b"),
    tags = list(c("x", "y"), c("z"))
  )

  write_duckdb_table(data, "test_table", db_path = db_path, overwrite = TRUE)
  stored <- read_duckdb_table("test_table", db_path = db_path)

  expect_equal(nrow(stored), 2)
  expect_true(is.character(stored$tags))
  expect_match(stored$tags[[1]], "x", fixed = TRUE)
})