expect_equal <- testthat::expect_equal
expect_match <- testthat::expect_match

test_that("get_validation_candidates keeps only queued rows and sorts by score", {
  normalized <- tibble::tibble(
    record_id = c("a", "b", "c"),
    name = c("alpha", "bravo", "charlie"),
    source = c("github", "github", "cisa_advisories"),
    source_type = c("repository", "repository", "rss_item"),
    url = c("https://example.com/a", "https://example.com/b", "https://example.com/c"),
    raw_description = c("A", "B", "C"),
    raw_text = c("A", "B", "C"),
    date_found = c("2026-04-03", "2026-04-04", "2026-04-05"),
    pre_llm_score = c(60, 90, 0),
    pre_llm_priority = c("medium", "high", "low"),
    pre_llm_candidate_type = c("utility", "utility_suite", "news_or_advisory"),
    pre_llm_should_process = c(TRUE, TRUE, FALSE),
    pre_llm_reasons = list("r1", "r2", "r3"),
    metadata = list(list(), list(), list())
  )

  candidates <- get_validation_candidates(normalized_data = normalized)

  expect_equal(candidates$record_id, c("b", "a"))
})

test_that("run_validation_enrichment saves enriched tools and skips prefiltered rows", {
  normalized <- tibble::tibble(
    record_id = c("repo-1", "rss-1"),
    name = c("sliver", "CVE roundup"),
    source = c("github", "cisa_advisories"),
    source_type = c("repository", "rss_item"),
    url = c("https://github.com/bishopfox/sliver", "https://example.com/cve"),
    raw_description = c("Adversary emulation framework", "Advisory summary"),
    raw_text = c("Adversary emulation framework and command and control platform", "Security advisory for CVE-2026-9999"),
    date_found = c("2026-04-04", "2026-04-04"),
    pre_llm_score = c(95, 0),
    pre_llm_priority = c("high", "low"),
    pre_llm_candidate_type = c("utility_suite", "news_or_advisory"),
    pre_llm_should_process = c(TRUE, FALSE),
    pre_llm_reasons = list(c("seed:sliver(+12)"), c("excluded:news(cve)")),
    metadata = list(list(topics = c("red-team")), list(feed_title = "CISA"))
  )

  target_env <- environment(run_validation_enrichment)
  original_completion <- get(".openai_chat_completion", envir = target_env, inherits = FALSE)
  binding_was_locked <- bindingIsLocked(".openai_chat_completion", target_env)
  tracker <- new.env(parent = emptyenv())
  tracker$call_count <- 0L

  if (binding_was_locked) {
    unlockBinding(".openai_chat_completion", target_env)
  }

  assign(
    ".openai_chat_completion",
    function(system_prompt, user_prompt, model, api_key, provider, base_url, ...) {
      tracker$call_count <- tracker$call_count + 1L
      testthat::expect_match(system_prompt, "real offensive security utility", fixed = TRUE)
      testthat::expect_match(user_prompt, "Candidate name: sliver", fixed = TRUE)
      testthat::expect_equal(provider, "deepseek")
      testthat::expect_equal(base_url, "https://api.deepseek.com")

      '{"is_tool":true,"name":"Sliver","purpose":"Command and control for adversary simulation","capabilities":["c2","payload delivery"],"target_platforms":["Windows","Linux"],"category":"c2_framework","reason":"Repository is a real offensive framework."}'
    },
    envir = target_env
  )

  on.exit({
    assign(".openai_chat_completion", original_completion, envir = target_env)
    if (binding_was_locked) {
      lockBinding(".openai_chat_completion", target_env)
    }
  }, add = TRUE)

  output_path <- tempfile(fileext = ".rds")
  enriched_path <- tempfile(fileext = ".rds")
  result <- run_validation_enrichment(
    normalized_data = normalized,
    output_path = output_path,
    enriched_output_path = enriched_path,
    provider = "deepseek",
    api_key = "test-key"
  )

  expect_equal(tracker$call_count, 1L)
  expect_true(file.exists(output_path))
  expect_true(file.exists(enriched_path))
  expect_equal(nrow(result$validation_results), 2)
  expect_equal(nrow(result$enriched_tools), 1)
  expect_equal(result$validation_results$llm_provider[[1]], "deepseek")
  expect_equal(result$validation_results$llm_base_url[[1]], "https://api.deepseek.com")
  expect_equal(result$enriched_tools$validated_name[[1]], "Sliver")
  expect_true(result$enriched_tools$is_tool[[1]])
  expect_equal(
    result$validation_results$llm_status[result$validation_results$record_id == "rss-1"],
    "skipped_pre_filter"
  )
})

test_that("run_validation_enrichment processes candidates in descending score order", {
  normalized <- tibble::tibble(
    record_id = c("tool-1", "tool-2"),
    name = c("lowtool", "hightool"),
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    url = c("https://example.com/lowtool", "https://example.com/hightool"),
    raw_description = c("tool one", "tool two"),
    raw_text = c("Offensive tool one", "Offensive tool two"),
    date_found = c("2026-04-04", "2026-04-05"),
    pre_llm_score = c(55, 90),
    pre_llm_priority = c("medium", "high"),
    pre_llm_candidate_type = c("utility", "utility_suite"),
    pre_llm_should_process = c(TRUE, TRUE),
    pre_llm_reasons = list(c("utility"), c("seed")),
    metadata = list(list(), list())
  )

  target_env <- environment(run_validation_enrichment)
  original_completion <- get(".openai_chat_completion", envir = target_env, inherits = FALSE)
  binding_was_locked <- bindingIsLocked(".openai_chat_completion", target_env)
  tracker <- new.env(parent = emptyenv())
  tracker$seen_names <- character(0)

  if (binding_was_locked) {
    unlockBinding(".openai_chat_completion", target_env)
  }

  assign(
    ".openai_chat_completion",
    function(system_prompt, user_prompt, model, api_key, provider, base_url, ...) {
      candidate_line <- strsplit(user_prompt, "\n", fixed = TRUE)[[1]][[1]]
      tracker$seen_names <- c(tracker$seen_names, sub("^Candidate name: ", "", candidate_line))
      testthat::expect_equal(provider, "deepseek")
      testthat::expect_equal(base_url, "https://api.deepseek.com")

      sprintf(
        '{"is_tool":true,"name":"%s","purpose":"Purpose","capabilities":["cap"],"target_platforms":["Windows"],"category":"utility","reason":"ok"}',
        sub("^Candidate name: ", "", candidate_line)
      )
    },
    envir = target_env
  )

  on.exit({
    assign(".openai_chat_completion", original_completion, envir = target_env)
    if (binding_was_locked) {
      lockBinding(".openai_chat_completion", target_env)
    }
  }, add = TRUE)

  run_validation_enrichment(
    normalized_data = normalized,
    output_path = tempfile(fileext = ".rds"),
    enriched_output_path = tempfile(fileext = ".rds"),
    provider = "deepseek",
    api_key = "test-key"
  )

  expect_equal(tracker$seen_names, c("hightool", "lowtool"))
})

test_that("get_llm_runtime_config reports non-secret OpenAI runtime settings", {
  original_provider <- Sys.getenv("LLM_PROVIDER", unset = NA_character_)
  original_model <- Sys.getenv("LLM_MODEL", unset = NA_character_)
  original_base_url <- Sys.getenv("LLM_BASE_URL", unset = NA_character_)
  original_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
  original_dotenv_path <- getOption("offensivetoolmapper.dotenv_path", NULL)

  Sys.unsetenv(c("LLM_PROVIDER", "LLM_MODEL", "LLM_BASE_URL"))
  Sys.setenv(OPENAI_API_KEY = "masked-test-key")
  options(offensivetoolmapper.dotenv_path = tempfile())

  on.exit({
    if (is.null(original_dotenv_path)) options(offensivetoolmapper.dotenv_path = NULL) else options(offensivetoolmapper.dotenv_path = original_dotenv_path)
    if (is.na(original_provider)) Sys.unsetenv("LLM_PROVIDER") else Sys.setenv(LLM_PROVIDER = original_provider)
    if (is.na(original_model)) Sys.unsetenv("LLM_MODEL") else Sys.setenv(LLM_MODEL = original_model)
    if (is.na(original_base_url)) Sys.unsetenv("LLM_BASE_URL") else Sys.setenv(LLM_BASE_URL = original_base_url)
    if (is.na(original_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = original_key)
  }, add = TRUE)

  config <- get_llm_runtime_config()

  expect_equal(config$provider[[1]], "openai")
  expect_equal(config$model[[1]], "gpt-4.1-mini")
  expect_equal(config$base_url[[1]], "https://api.openai.com/v1")
  expect_true(config$api_key_present[[1]])
})

test_that("dotenv fallback can switch the LLM provider", {
  dotenv_path <- tempfile(fileext = ".env")
  writeLines(c("LLM_PROVIDER=deepseek", "DEEPSEEK_API_KEY=masked-test-key"), dotenv_path, useBytes = TRUE)

  original_dotenv_path <- getOption("offensivetoolmapper.dotenv_path", NULL)
  original_provider <- Sys.getenv("LLM_PROVIDER", unset = NA_character_)
  original_key <- Sys.getenv("DEEPSEEK_API_KEY", unset = NA_character_)

  Sys.unsetenv(c("LLM_PROVIDER", "DEEPSEEK_API_KEY"))
  options(offensivetoolmapper.dotenv_path = dotenv_path)

  on.exit({
    if (is.null(original_dotenv_path)) options(offensivetoolmapper.dotenv_path = NULL) else options(offensivetoolmapper.dotenv_path = original_dotenv_path)
    if (is.na(original_provider)) Sys.unsetenv("LLM_PROVIDER") else Sys.setenv(LLM_PROVIDER = original_provider)
    if (is.na(original_key)) Sys.unsetenv("DEEPSEEK_API_KEY") else Sys.setenv(DEEPSEEK_API_KEY = original_key)
  }, add = TRUE)

  expect_equal(get_default_llm_provider(), "deepseek")
  expect_equal(get_llm_api_key("deepseek"), "masked-test-key")
})
