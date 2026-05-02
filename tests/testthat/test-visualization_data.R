test_that("build_visualization_dataset creates UI-facing fields and MITRE tags", {
  assessment_results <- tibble::tibble(
    record_id = c("repo-1", "repo-2"),
    name = c("sliver", "news-item"),
    source = c("github", "rss"),
    source_type = c("repository", "rss_item"),
    url = c("https://github.com/bishopfox/sliver", "https://example.com/news"),
    raw_description = c("Adversary emulation framework", "Advisory"),
    raw_text = c("sliver raw", "news raw"),
    llm_input_text = c("sliver input", "news input"),
    date_found = c("2026-04-04", "2026-04-04"),
    pre_llm_score = c(95, 0),
    pre_llm_priority = c("high", "low"),
    pre_llm_candidate_type = c("utility_suite", "news_or_advisory"),
    pre_llm_should_process = c(TRUE, FALSE),
    pre_llm_reasons = list(c("seed:sliver(+12)"), c("excluded:news(cve)")),
    metadata = list(list(), list()),
    llm_provider = c("deepseek", "deepseek"),
    llm_base_url = c("https://api.deepseek.com", "https://api.deepseek.com"),
    llm_model = c("deepseek-chat", NA_character_),
    llm_processed_at = c("2026-04-04 22:00:00", "2026-04-04 22:00:00"),
    llm_status = c("success", "skipped_pre_filter"),
    llm_error = c(NA_character_, NA_character_),
    is_relevant = c(TRUE, NA),
    entity_type = c("framework", NA_character_),
    is_tool = c(TRUE, NA),
    assessed_name = c("Sliver", NA_character_),
    summary_ru = c("Краткое описание Sliver.", NA_character_),
    purpose_ru = c("Полное описание назначения Sliver.", NA_character_),
    capabilities_ru = list(c("командный канал", "пост-эксплуатация"), character(0)),
    target_platforms = list(c("Windows", "Linux"), character(0)),
    category_ru = c("C2 и пост-эксплуатация", NA_character_),
    overall_confidence = c(95, NA_real_),
    reason_ru = c("Подходит под offensive framework.", NA_character_)
  )

  mitre_mappings <- tibble::tibble(
    record_id = "repo-1",
    assessed_name = "Sliver",
    technique_id = c("T1071", "T1055"),
    technique_name = c("Application Layer Protocol", "Process Injection"),
    tactic = c("Command and Control", "Defense Evasion"),
    confidence = c(90, 80),
    reasoning_ru = c("C2 через application layer protocol.", "Инжект в процесс.")
  )

  output_path <- tempfile(fileext = ".rds")
  matrix_path <- tempfile(fileext = ".rds")
  duckdb_path <- tempfile(fileext = ".duckdb")

  result <- build_visualization_dataset(
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    output_path = output_path,
    matrix_output_path = matrix_path,
    duckdb_path = duckdb_path,
    write_duckdb = TRUE
  )

  expect_true(file.exists(output_path))
  expect_true(file.exists(matrix_path))
  expect_true(file.exists(duckdb_path))
  expect_equal(nrow(result$visualization_tools), 1)
  expect_equal(nrow(result$visualization_tool_matrix), 2)
  expect_equal(result$visualization_tools$assessed_name[[1]], "Sliver")
  expect_equal(result$visualization_tools$short_description_ru[[1]], "Краткое описание Sliver.")
  expect_match(result$visualization_tools$long_description_ru[[1]], "Полное описание назначения Sliver.", fixed = TRUE)
  expect_equal(result$visualization_tools$confidence_score[[1]], 0.95)
  expect_true(result$visualization_tools$visualization_score[[1]] > 0)
  expect_equal(result$visualization_tools$visualization_rank[[1]], 1)
  expect_true("mitre:technique:T1071" %in% result$visualization_tools$filter_tags[[1]])
  expect_true("mitre:tactic:command_and_control" %in% result$visualization_tools$filter_tags[[1]])

  stored_visualization <- read_duckdb_table("visualization_tools", db_path = duckdb_path)
  expect_equal(nrow(stored_visualization), 1)
})

test_that("frameworks with richer descriptions rank above thinner script collections", {
  assessment_results <- tibble::tibble(
    record_id = c("repo-1", "repo-2"),
    name = c("sliver", "helper-pack"),
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    url = c("https://github.com/bishopfox/sliver", "https://github.com/acme/helper-pack"),
    raw_description = c("Framework", "Scripts"),
    raw_text = c("sliver raw", "helper raw"),
    llm_input_text = c("sliver input", "helper input"),
    date_found = c("2026-04-04", "2026-04-04"),
    pre_llm_score = c(88, 90),
    pre_llm_priority = c("high", "high"),
    pre_llm_candidate_type = c("utility_suite", "script_collection"),
    pre_llm_should_process = c(TRUE, TRUE),
    pre_llm_reasons = list(c("seed"), c("script")),
    metadata = list(list(), list()),
    llm_provider = c("deepseek", "deepseek"),
    llm_base_url = c("https://api.deepseek.com", "https://api.deepseek.com"),
    llm_model = c("deepseek-chat", "deepseek-chat"),
    llm_processed_at = c("2026-04-04 22:00:00", "2026-04-04 22:00:00"),
    llm_status = c("success", "success"),
    llm_error = c(NA_character_, NA_character_),
    is_relevant = c(TRUE, TRUE),
    entity_type = c("framework", "script_collection"),
    is_tool = c(TRUE, TRUE),
    assessed_name = c("Sliver", "Helper Pack"),
    summary_ru = c("Подробный фреймворк для red team.", "Набор скриптов."),
    purpose_ru = c("Длинное и содержательное описание offensive framework.", "Короткое описание."),
    capabilities_ru = list(c("c2", "post-exploitation", "pivoting"), c("scripts")),
    target_platforms = list(c("Windows", "Linux"), c("Windows")),
    category_ru = c("C2 и пост-эксплуатация", "Скрипты"),
    overall_confidence = c(95, 85),
    reason_ru = c("Сильный offensive signal.", "Полезный набор." )
  )

  mitre_mappings <- tibble::tibble(
    record_id = c("repo-1", "repo-1", "repo-2"),
    assessed_name = c("Sliver", "Sliver", "Helper Pack"),
    technique_id = c("T1071", "T1055", "T1595"),
    technique_name = c("Application Layer Protocol", "Process Injection", "Active Scanning"),
    tactic = c("Command and Control", "Defense Evasion", "Reconnaissance"),
    confidence = c(0.9, 0.8, 0.6),
    reasoning_ru = c("a", "b", "c")
  )

  result <- build_visualization_dataset(
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    output_path = tempfile(fileext = ".rds"),
    matrix_output_path = tempfile(fileext = ".rds"),
    duckdb_path = tempfile(fileext = ".duckdb"),
    write_duckdb = FALSE
  )

  expect_equal(result$visualization_tools$assessed_name[[1]], "Sliver")
  expect_true(result$visualization_tools$visualization_score[[1]] > result$visualization_tools$visualization_score[[2]])
})

test_that("build_visualization_dataset excludes documentation-like repositories from UI output", {
  assessment_results <- tibble::tibble(
    record_id = c("repo-1", "repo-2"),
    name = c("sliver", "sliver-cheatsheet"),
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    url = c("https://github.com/bishopfox/sliver", "https://github.com/anon/sliver-cheatsheet"),
    raw_description = c("Framework", "Cheat sheet"),
    raw_text = c("sliver raw", "cheat sheet raw"),
    llm_input_text = c("sliver input", "cheat sheet input"),
    date_found = c("2026-04-04", "2026-04-04"),
    pre_llm_score = c(95, 80),
    pre_llm_priority = c("high", "high"),
    pre_llm_candidate_type = c("utility_suite", "utility"),
    pre_llm_should_process = c(TRUE, TRUE),
    pre_llm_reasons = list(c("seed"), c("candidate")),
    metadata = list(list(), list(topics = c("cheatsheet"))),
    llm_provider = c("deepseek", "deepseek"),
    llm_base_url = c("https://api.deepseek.com", "https://api.deepseek.com"),
    llm_model = c("deepseek-chat", "deepseek-chat"),
    llm_processed_at = c("2026-04-04 22:00:00", "2026-04-04 22:00:00"),
    llm_status = c("success", "success"),
    llm_error = c(NA_character_, NA_character_),
    is_relevant = c(TRUE, TRUE),
    entity_type = c("framework", "utility_suite"),
    is_tool = c(TRUE, TRUE),
    assessed_name = c("Sliver", "sliver-cheatsheet"),
    summary_ru = c("Краткое описание Sliver.", "Шпаргалка по командам Sliver."),
    purpose_ru = c("Полное описание назначения Sliver.", "Подготовка к экзамену и быстрый справочник."),
    capabilities_ru = list(c("командный канал", "пост-эксплуатация"), c("команды")),
    target_platforms = list(c("Windows", "Linux"), c("Windows")),
    category_ru = c("C2 и пост-эксплуатация", "Шпаргалка"),
    overall_confidence = c(95, 90),
    reason_ru = c("Подходит под offensive framework.", "Репозиторий выглядит как cheat sheet.")
  )

  result <- build_visualization_dataset(
    assessment_results = assessment_results,
    mitre_mappings = tibble::tibble(
      record_id = character(),
      assessed_name = character(),
      technique_id = character(),
      technique_name = character(),
      tactic = character(),
      confidence = numeric(),
      reasoning_ru = character()
    ),
    output_path = tempfile(fileext = ".rds"),
    matrix_output_path = tempfile(fileext = ".rds"),
    duckdb_path = tempfile(fileext = ".duckdb"),
    write_duckdb = FALSE
  )

  expect_equal(nrow(result$visualization_tools), 1)
  expect_equal(result$visualization_tools$assessed_name[[1]], "Sliver")
})

test_that("build_visualization_dataset creates module mappings for large suites", {
  assessment_results <- tibble::tibble(
    record_id = "repo-metasploit",
    name = "metasploit-framework",
    source = "github",
    source_type = "repository",
    url = "https://github.com/rapid7/metasploit-framework",
    raw_description = "Metasploit Framework",
    raw_text = "Metasploit Framework exploit payload post auxiliary scanner modules",
    llm_input_text = "metasploit input",
    date_found = "2026-04-04",
    pre_llm_score = 95,
    pre_llm_priority = "high",
    pre_llm_candidate_type = "utility_suite",
    pre_llm_should_process = TRUE,
    pre_llm_reasons = list(c("seed:metasploit")),
    metadata = list(list()),
    llm_provider = "deepseek",
    llm_base_url = "https://api.deepseek.com",
    llm_model = "deepseek-chat",
    llm_processed_at = "2026-04-04 22:00:00",
    llm_status = "success",
    llm_error = NA_character_,
    is_relevant = TRUE,
    entity_type = "framework",
    is_tool = TRUE,
    assessed_name = "Metasploit Framework",
    summary_ru = "Metasploit Framework.",
    purpose_ru = "Large offensive framework with exploit, payload, post, and auxiliary modules.",
    capabilities_ru = list(c("exploit modules", "payloads", "post-exploitation", "auxiliary scanners")),
    target_platforms = list(c("Windows", "Linux")),
    category_ru = "offensive framework",
    overall_confidence = 95,
    reason_ru = "Known offensive framework."
  )

  mitre_mappings <- tibble::tibble(
    record_id = c("repo-metasploit", "repo-metasploit", "repo-metasploit", "repo-metasploit"),
    assessed_name = rep("Metasploit Framework", 4),
    technique_id = c("T1190", "T1071", "T1003", "T1046"),
    technique_name = c("Exploit Public-Facing Application", "Application Layer Protocol", "OS Credential Dumping", "Network Service Discovery"),
    tactic = c("Initial Access", "Command and Control", "Credential Access", "Discovery"),
    confidence = c(0.9, 0.8, 0.85, 0.7),
    reasoning_ru = c("exploit module", "payload callback", "post module", "auxiliary scanner")
  )

  result <- build_visualization_dataset(
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    output_path = tempfile(fileext = ".rds"),
    matrix_output_path = tempfile(fileext = ".rds"),
    modules_output_path = tempfile(fileext = ".rds"),
    duckdb_path = tempfile(fileext = ".duckdb"),
    write_duckdb = FALSE
  )

  expect_true(nrow(result$visualization_tool_modules) >= 4)
  expect_true("metasploit_exploit_modules" %in% result$visualization_tool_modules$module_id)
  expect_true("metasploit_payload_handlers" %in% result$visualization_tool_modules$module_id)
  expect_true("metasploit_post_modules" %in% result$visualization_tool_modules$module_id)
  expect_true("metasploit_auxiliary_scanners" %in% result$visualization_tool_modules$module_id)
})
