expect_equal <- testthat::expect_equal
expect_match <- testthat::expect_match

test_that("unified assessment schema exposes Russian UI fields", {
  schema <- get_unified_tool_assessment_schema()

  expect_equal(schema$type, "object")
  expect_true("summary_ru" %in% schema$required)
  expect_true("mitre_classifications" %in% names(schema$properties))
})

test_that("parse_unified_tool_assessment_json validates one-call payloads", {
  payload <- paste0(
    '{',
    '"is_relevant": true,',
    '"entity_type": "framework",',
    '"is_tool": true,',
    '"name": "Sliver",',
    '"summary_ru": "Кроссплатформенный red team фреймворк.",',
    '"purpose_ru": "Используется для adversary emulation и C2.",',
    '"capabilities_ru": ["командный канал", "пост-эксплуатация"],',
    '"target_platforms": ["Windows", "Linux"],',
    '"category_ru": "C2 и пост-эксплуатация",',
    '"overall_confidence": 0.91,',
    '"reason_ru": "Описание явно указывает на offensive framework.",',
    '"mitre_classifications": [',
    '{"technique_id": "T1071", "technique_name": "Application Layer Protocol", "tactic": "Command and Control", "confidence": 0.82, "reasoning_ru": "Фреймворк использует application layer protocol для C2."}',
    ']',
    '}'
  )

  result <- parse_unified_tool_assessment_json(payload)

  expect_equal(result$assessment$entity_type[[1]], "framework")
  expect_equal(result$assessment$summary_ru[[1]], "Кроссплатформенный red team фреймворк.")
  expect_equal(result$mitre_classifications$technique_id[[1]], "T1071")
})

test_that("parse_unified_tool_assessment_json accepts DeepSeek-style aliases", {
  payload <- paste0(
    '{',
    '"relevant": true,',
    '"entity_type": "utility_suite",',
    '"summary_ru": "Набор offensive-утилит.",',
    '"purpose_ru": "Автоматизация offensive задач.",',
    '"capabilities_ru": "разведка и эксплуатация",',
    '"target_platforms": ["Windows", "Linux"],',
    '"category_ru": "Автоматизация пентестинга",',
    '"reason_ru": "Репозиторий явно описывает offensive automation.",',
    '"mitre_attack": [',
    '{"technique_id": "T1595", "technique_name": "Active Scanning", "tactic": "Reconnaissance"}',
    ']',
    '}'
  )

  result <- parse_unified_tool_assessment_json(payload, fallback_name = "pentest-ai-agents")

  expect_true(result$assessment$is_relevant[[1]])
  expect_true(result$assessment$is_tool[[1]])
  expect_equal(result$assessment$name[[1]], "pentest-ai-agents")
  expect_equal(result$assessment$overall_confidence[[1]], 0.75)
  expect_equal(result$mitre_classifications$technique_id[[1]], "T1595")
  expect_equal(result$mitre_classifications$confidence[[1]], 0.75)
})

test_that("DeepSeek is the default LLM provider configuration", {
  original_provider <- Sys.getenv("LLM_PROVIDER", unset = NA_character_)
  original_model <- Sys.getenv("LLM_MODEL", unset = NA_character_)
  original_base_url <- Sys.getenv("LLM_BASE_URL", unset = NA_character_)

  Sys.unsetenv(c("LLM_PROVIDER", "LLM_MODEL", "LLM_BASE_URL"))

  on.exit({
    if (is.na(original_provider)) {
      Sys.unsetenv("LLM_PROVIDER")
    } else {
      Sys.setenv(LLM_PROVIDER = original_provider)
    }

    if (is.na(original_model)) {
      Sys.unsetenv("LLM_MODEL")
    } else {
      Sys.setenv(LLM_MODEL = original_model)
    }

    if (is.na(original_base_url)) {
      Sys.unsetenv("LLM_BASE_URL")
    } else {
      Sys.setenv(LLM_BASE_URL = original_base_url)
    }
  }, add = TRUE)

  expect_equal(get_default_llm_provider(), "deepseek")
  expect_equal(get_default_llm_model(), "deepseek-chat")
  expect_equal(get_default_llm_base_url(), "https://api.deepseek.com")
})

test_that("run_unified_tool_assessment saves assessment, relevant tools, and MITRE rows", {
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

  target_env <- environment(run_unified_tool_assessment)
  original_completion <- get(".openai_chat_completion", envir = target_env, inherits = FALSE)
  binding_was_locked <- bindingIsLocked(".openai_chat_completion", target_env)

  if (binding_was_locked) {
    unlockBinding(".openai_chat_completion", target_env)
  }

  assign(
    ".openai_chat_completion",
    function(system_prompt, user_prompt, model, api_key, provider, base_url, schema_name, schema) {
      testthat::expect_match(system_prompt, "Russian is required for summary_ru", fixed = TRUE)
      testthat::expect_equal(provider, "deepseek")
      testthat::expect_equal(base_url, "https://api.deepseek.com")
      testthat::expect_equal(schema_name, "unified_tool_assessment")
      testthat::expect_match(user_prompt, "Candidate name: sliver", fixed = TRUE)

      paste0(
        '{',
        '"is_relevant": true,',
        '"entity_type": "framework",',
        '"is_tool": true,',
        '"name": "Sliver",',
        '"summary_ru": "Кроссплатформенный red team фреймворк для C2 и пост-эксплуатации.",',
        '"purpose_ru": "Используется для adversary emulation, управления имплантами и C2.",',
        '"capabilities_ru": ["командный канал", "генерация имплантов"],',
        '"target_platforms": ["Windows", "Linux", "macOS"],',
        '"category_ru": "C2 и пост-эксплуатация",',
        '"overall_confidence": 0.95,',
        '"reason_ru": "Описание и метаданные явно указывают на offensive framework.",',
        '"mitre_classifications": [',
        '{"technique_id": "T1071", "technique_name": "Application Layer Protocol", "tactic": "Command and Control", "confidence": 0.85, "reasoning_ru": "Фреймворк использует application layer protocol для C2."}',
        ']',
        '}'
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

  output_path <- tempfile(fileext = ".rds")
  relevant_path <- tempfile(fileext = ".rds")
  mitre_path <- tempfile(fileext = ".rds")
  duckdb_path <- tempfile(fileext = ".duckdb")

  result <- run_unified_tool_assessment(
    normalized_data = normalized,
    output_path = output_path,
    relevant_output_path = relevant_path,
    mitre_output_path = mitre_path,
    duckdb_path = duckdb_path,
    write_duckdb = TRUE,
    api_key = "test-key"
  )

  expect_true(file.exists(output_path))
  expect_true(file.exists(relevant_path))
  expect_true(file.exists(mitre_path))
  expect_true(file.exists(duckdb_path))
  expect_equal(nrow(result$assessment_results), 2)
  expect_equal(nrow(result$relevant_tools), 1)
  expect_equal(nrow(result$mitre_mappings), 1)
  expect_equal(result$assessment_results$llm_provider[[1]], "deepseek")
  expect_equal(result$assessment_results$llm_base_url[[1]], "https://api.deepseek.com")
  expect_equal(result$relevant_tools$summary_ru[[1]], "Кроссплатформенный red team фреймворк для C2 и пост-эксплуатации.")
  expect_equal(result$mitre_mappings$technique_id[[1]], "T1071")

  stored_assessments <- read_duckdb_table("llm_assessments", db_path = duckdb_path)
  expect_equal(nrow(stored_assessments), 2)
})

test_that("build_unified_tool_assessment_prompt uses cleaned input text", {
  record <- tibble::tibble(
    record_id = "x",
    name = "toolkit",
    source = "github",
    source_type = "repository",
    url = "https://github.com/acme/toolkit",
    raw_description = "desc",
    raw_text = "# Toolkit\n![badge](https://example.com/badge)\nUseful offensive utility",
    date_found = "2026-04-04",
    pre_llm_score = 50,
    pre_llm_priority = "medium",
    pre_llm_candidate_type = "utility",
    pre_llm_should_process = TRUE,
    pre_llm_reasons = list(c("candidate:utility")),
    metadata = list(list(topics = c("red-team")))
  )

  prompt <- build_unified_tool_assessment_prompt(record)

  expect_match(prompt, "Useful offensive utility", fixed = TRUE)
  expect_false(grepl("badge", prompt, fixed = TRUE))
})

test_that("run_unified_tool_assessment drops documentation-like repos even if LLM marks them relevant", {
  normalized <- tibble::tibble(
    record_id = c("repo-1", "repo-2"),
    name = c("sliver", "sliver-cheatsheet"),
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    url = c("https://github.com/bishopfox/sliver", "https://github.com/anon/sliver-cheatsheet"),
    raw_description = c("Framework", "Cheat sheet"),
    raw_text = c("Adversary emulation framework and C2 platform", "Cheat sheet with Sliver commands and exam notes"),
    date_found = c("2026-04-04", "2026-04-04"),
    pre_llm_score = c(95, 80),
    pre_llm_priority = c("high", "high"),
    pre_llm_candidate_type = c("utility_suite", "utility"),
    pre_llm_should_process = c(TRUE, TRUE),
    pre_llm_reasons = list(c("seed:sliver(+12)"), c("candidate:utility(tool)")),
    metadata = list(list(topics = c("red-team")), list(topics = c("cheatsheet")))
  )

  target_env <- environment(run_unified_tool_assessment)
  original_completion <- get(".openai_chat_completion", envir = target_env, inherits = FALSE)
  binding_was_locked <- bindingIsLocked(".openai_chat_completion", target_env)

  if (binding_was_locked) {
    unlockBinding(".openai_chat_completion", target_env)
  }

  assign(
    ".openai_chat_completion",
    function(system_prompt, user_prompt, model, api_key, provider, base_url, schema_name, schema) {
      candidate_name <- if (grepl("Candidate name: sliver-cheatsheet", user_prompt, fixed = TRUE)) "sliver-cheatsheet" else "Sliver"

      paste0(
        '{',
        '"is_relevant": true,',
        '"entity_type": "utility_suite",',
        '"is_tool": true,',
        '"name": "', candidate_name, '",',
        '"summary_ru": "Описание offensive utility.",',
        '"purpose_ru": "Назначение offensive utility.",',
        '"capabilities_ru": ["команда"],',
        '"target_platforms": ["Windows"],',
        '"category_ru": "Тестирование",',
        '"overall_confidence": 0.90,',
        '"reason_ru": "Похоже на offensive candidate.",',
        '"mitre_classifications": []',
        '}'
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

  result <- run_unified_tool_assessment(
    normalized_data = normalized,
    output_path = tempfile(fileext = ".rds"),
    relevant_output_path = tempfile(fileext = ".rds"),
    mitre_output_path = tempfile(fileext = ".rds"),
    duckdb_path = tempfile(fileext = ".duckdb"),
    write_duckdb = FALSE,
    api_key = "test-key"
  )

  expect_equal(nrow(result$assessment_results), 2)
  expect_equal(nrow(result$relevant_tools), 1)
  expect_equal(result$relevant_tools$assessed_name[[1]], "Sliver")
})