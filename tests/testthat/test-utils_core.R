test_that("ensure_dir creates directories recursively", {
  temp_dir <- tempfile(pattern = "offensivetoolmapper-")

  expect_false(dir.exists(temp_dir))
  ensure_dir(temp_dir)
  expect_true(dir.exists(temp_dir))
})

test_that("load_pipeline_rds returns NULL for optional missing files", {
  missing_file <- tempfile(fileext = ".rds")
  expect_null(load_pipeline_rds(missing_file, required = FALSE))
})

test_that("require_env_var errors for missing variables", {
  expect_error(require_env_var("OFFENSIVETOOLMAPPER_TEST_MISSING"))
})