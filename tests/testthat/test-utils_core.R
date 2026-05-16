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

test_that("runtime env reads local dotenv before process env", {
  dotenv_path <- tempfile(fileext = ".env")
  writeLines(c("OPENAI_API_KEY=dotenv-key", "LLM_PROVIDER=deepseek"), dotenv_path, useBytes = TRUE)

  original_dotenv_path <- getOption("offensivetoolmapper.dotenv_path", NULL)
  original_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
  original_provider <- Sys.getenv("LLM_PROVIDER", unset = NA_character_)

  Sys.setenv(OPENAI_API_KEY = "system-key", LLM_PROVIDER = "openai")
  options(offensivetoolmapper.dotenv_path = dotenv_path)

  on.exit({
    if (is.null(original_dotenv_path)) options(offensivetoolmapper.dotenv_path = NULL) else options(offensivetoolmapper.dotenv_path = original_dotenv_path)
    if (is.na(original_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = original_key)
    if (is.na(original_provider)) Sys.unsetenv("LLM_PROVIDER") else Sys.setenv(LLM_PROVIDER = original_provider)
  }, add = TRUE)

  expect_equal(get_runtime_env_value("OPENAI_API_KEY"), "dotenv-key")
  expect_equal(get_runtime_env_value("LLM_PROVIDER"), "deepseek")
})

test_that("empty local dotenv entries mask stale process env values", {
  dotenv_path <- tempfile(fileext = ".env")
  writeLines("GITHUB_PAT=", dotenv_path, useBytes = TRUE)

  original_dotenv_path <- getOption("offensivetoolmapper.dotenv_path", NULL)
  original_pat <- Sys.getenv("GITHUB_PAT", unset = NA_character_)

  Sys.setenv(GITHUB_PAT = "system-token")
  options(offensivetoolmapper.dotenv_path = dotenv_path)

  on.exit({
    if (is.null(original_dotenv_path)) options(offensivetoolmapper.dotenv_path = NULL) else options(offensivetoolmapper.dotenv_path = original_dotenv_path)
    if (is.na(original_pat)) Sys.unsetenv("GITHUB_PAT") else Sys.setenv(GITHUB_PAT = original_pat)
  }, add = TRUE)

  expect_equal(get_runtime_env_value("GITHUB_PAT", unset = ""), "")
})
