test_that("run_pipeline_from can normalize from supplied raw inputs and persist stage status", {
  data_dir <- tempfile(pattern = "pipeline-data-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  github_raw <- tibble::tibble(
    owner = "acme",
    repo = "sliver-like",
    full_name = "acme/sliver-like",
    html_url = "https://github.com/acme/sliver-like",
    api_url = "https://api.github.com/repos/acme/sliver-like",
    description = "Offensive red team framework",
    language = "Go",
    topics = list(c("red-team", "c2")),
    stargazers_count = 150L,
    forks_count = 10L,
    open_issues_count = 1L,
    archived = FALSE,
    readme = "Framework for command and control operations.",
    matched_queries = list(c("red team tool")),
    matched_query_text = "red team tool",
    matched_search_modes = list(c("stars")),
    matched_search_mode_text = "stars",
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = "https://example.com/sliver-like",
    updated_at = "2026-04-05T00:00:00Z",
    pushed_at = "2026-04-05T00:00:00Z",
    search_score = 12,
    source_type = "repository"
  )

  result <- run_pipeline_from(
    stage = "normalize",
    end_stage = "normalize",
    data_dir = data_dir,
    raw_inputs = list(github = github_raw, packetstorm = NULL, rss = NULL),
    write_duckdb = FALSE
  )

  expect_true(file.exists(file.path(data_dir, "normalized_tools.rds")))
  expect_true(file.exists(file.path(data_dir, "pipeline_status.rds")))
  expect_equal(nrow(result$normalized_data), 1)
  expect_equal(nrow(result$pipeline_status), 2)
  expect_equal(result$pipeline_status$stage, c("normalize", "sanity_checks"))
  expect_true(all(result$pipeline_status$status == "success"))
})

test_that("run_pipeline_from can resume at visualize using supplied assessment artifacts", {
  data_dir <- tempfile(pattern = "pipeline-data-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  assessment_results <- tibble::tibble(
    record_id = "repo-1",
    name = "sliver",
    source = "github",
    source_type = "repository",
    url = "https://github.com/bishopfox/sliver",
    raw_description = "Framework",
    raw_text = "framework raw",
    llm_input_text = "framework raw",
    date_found = "2026-04-05",
    pre_llm_score = 95,
    pre_llm_priority = "high",
    pre_llm_candidate_type = "utility_suite",
    pre_llm_should_process = TRUE,
    pre_llm_reasons = list(c("seed:sliver")),
    metadata = list(list()),
    llm_provider = "deepseek",
    llm_base_url = "https://api.deepseek.com",
    llm_model = "deepseek-chat",
    llm_processed_at = "2026-04-05 10:00:00",
    llm_status = "success",
    llm_error = NA_character_,
    is_relevant = TRUE,
    entity_type = "framework",
    is_tool = TRUE,
    assessed_name = "Sliver",
    summary_ru = "Краткое описание Sliver.",
    purpose_ru = "Полное описание назначения Sliver.",
    capabilities_ru = list(c("командный канал", "пост-эксплуатация")),
    target_platforms = list(c("Windows", "Linux")),
    category_ru = "C2 и пост-эксплуатация",
    overall_confidence = 0.95,
    reason_ru = "Подходит под offensive framework."
  )

  mitre_mappings <- tibble::tibble(
    record_id = "repo-1",
    assessed_name = "Sliver",
    technique_id = c("T1071", "T1055"),
    technique_name = c("Application Layer Protocol", "Process Injection"),
    tactic = c("Command and Control", "Defense Evasion"),
    confidence = c(0.9, 0.8),
    reasoning_ru = c("a", "b")
  )

  result <- run_pipeline_from(
    stage = "visualize",
    end_stage = "visualize",
    data_dir = data_dir,
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    write_duckdb = FALSE
  )

  expect_true(file.exists(file.path(data_dir, "visualization_tools.rds")))
  expect_true(file.exists(file.path(data_dir, "visualization_tool_matrix.rds")))
  expect_equal(nrow(result$visualization_tools), 1)
  expect_equal(nrow(result$visualization_tool_matrix), 2)
  expect_equal(result$pipeline_status$stage[[1]], "visualize")
})

test_that("run_pipeline_from can execute the optional MITRE refinement stage", {
  data_dir <- tempfile(pattern = "pipeline-data-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  assessment_results <- tibble::tibble(
    record_id = "repo-1",
    name = "phisher",
    source = "github",
    source_type = "repository",
    url = "https://example.com/phisher",
    raw_description = "Phishing utility",
    raw_text = "This utility sends phishing links and phishing messages to obtain credentials.",
    llm_input_text = "This utility sends phishing links and phishing messages to obtain credentials.",
    date_found = "2026-04-05",
    pre_llm_score = 85,
    pre_llm_priority = "high",
    pre_llm_candidate_type = "utility",
    pre_llm_should_process = TRUE,
    pre_llm_reasons = list(c("candidate:utility")),
    metadata = list(list()),
    llm_provider = "deepseek",
    llm_base_url = "https://api.deepseek.com",
    llm_model = "deepseek-chat",
    llm_processed_at = "2026-04-05 12:00:00",
    llm_status = "success",
    llm_error = NA_character_,
    is_relevant = TRUE,
    entity_type = "utility",
    is_tool = TRUE,
    assessed_name = "Phisher",
    summary_ru = "Фишинговая утилита.",
    purpose_ru = "Помогает проводить phishing кампании.",
    capabilities_ru = list(c("phishing")),
    target_platforms = list(c("Windows")),
    category_ru = "Initial access",
    overall_confidence = 0.91,
    reason_ru = "Имеет прямой phishing signal."
  )

  mitre_mappings <- tibble::tibble(
    record_id = "repo-1",
    assessed_name = "Phisher",
    technique_id = "T1566",
    technique_name = "Phishing",
    tactic = "Initial Access",
    confidence = 0.88,
    reasoning_ru = "Phishing behavior"
  )

  mitre_attack <- tibble::tibble(
    stix_id = c("attack-pattern--001", "attack-pattern--002"),
    technique_id = c("T1566", "T1055"),
    technique_name = c("Phishing", "Process Injection"),
    tactic_shortname = c("initial-access", "defense-evasion"),
    description = c(
      "Send a phishing message or link to gain access to credentials and user systems.",
      "Inject code into another process to evade defenses or execute payloads."
    ),
    is_subtechnique = c(FALSE, FALSE),
    platforms = c("Windows; Linux", "Windows; Linux"),
    created = c("2024-01-01", "2024-01-01"),
    modified = c("2024-01-02", "2024-01-02"),
    tactic_id = c("TA0001", "TA0005"),
    tactic_name = c("Initial Access", "Defense Evasion"),
    tactic_description = c("Gain initial access.", "Avoid detection."),
    tactic_created = c("2024-01-01", "2024-01-01"),
    tactic_modified = c("2024-01-02", "2024-01-02")
  )

  result <- run_pipeline_from(
    stage = "refine_mitre",
    end_stage = "refine_mitre",
    data_dir = data_dir,
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    mitre_attack = mitre_attack,
    run_mitre_refinement = TRUE,
    write_duckdb = FALSE
  )

  expect_true(file.exists(file.path(data_dir, "mitre_refinement_candidates.rds")))
  expect_true(file.exists(file.path(data_dir, "mitre_refinement_index.rds")))
  expect_true(nrow(result$mitre_refinement_candidates) >= 1)
  expect_equal(result$pipeline_status$stage[[1]], "refine_mitre")
})
