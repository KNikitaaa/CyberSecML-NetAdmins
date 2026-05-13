make_test_mitre_attack_matrix <- function() {
  tibble::tibble(
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
}

test_that("retrieve_relevant_techniques returns the best lexical MITRE candidates", {
  data_dir <- tempfile(pattern = "mitre-refine-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  index <- build_mitre_refinement_index(
    mitre_attack = make_test_mitre_attack_matrix(),
    data_dir = data_dir,
    output_path = file.path(data_dir, "mitre_refinement_index.rds"),
    overwrite = TRUE
  )

  result <- retrieve_relevant_techniques(
    query_text = "Tool sends phishing links and phishing messages to gain initial access.",
    mitre_index = index,
    top_k = 2,
    min_score = 0.01
  )

  expect_true(nrow(result) >= 1)
  expect_equal(result$technique_id[[1]], "T1566")
  expect_true(file.exists(file.path(data_dir, "mitre_refinement_index.rds")))
})

test_that("run_mitre_refinement marks already mapped techniques and saves candidates", {
  data_dir <- tempfile(pattern = "mitre-refine-")
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

  result <- run_mitre_refinement(
    assessment_results = assessment_results,
    mitre_mappings = mitre_mappings,
    mitre_attack = make_test_mitre_attack_matrix(),
    data_dir = data_dir,
    index_path = file.path(data_dir, "mitre_refinement_index.rds"),
    output_path = file.path(data_dir, "mitre_refinement_candidates.rds"),
    top_k = 3,
    min_score = 0.01,
    overwrite_index = TRUE
  )

  expect_true(file.exists(file.path(data_dir, "mitre_refinement_candidates.rds")))
  expect_true(nrow(result) >= 1)
  expect_equal(result$record_id[[1]], "repo-1")
  expect_true(any(result$already_mapped))
  expect_equal(result$technique_id[result$already_mapped][[1]], "T1566")
})