test_that("plot helpers return ggplot objects for visualization datasets", {
  visualization_tools <- tibble::tibble(
    assessed_name = c("Sliver", "RedTeam-Tools"),
    source = c("github", "github"),
    entity_type = c("framework", "script_collection"),
    visualization_score = c(0.92, 0.74),
    confidence_score = c(0.95, 0.80)
  )

  visualization_matrix <- tibble::tibble(
    record_id = c("repo-1", "repo-1", "repo-2"),
    assessed_name = c("Sliver", "Sliver", "RedTeam-Tools"),
    technique_id = c("T1071", "T1055", "T1566"),
    technique_name = c("Application Layer Protocol", "Process Injection", "Phishing"),
    tactic = c("Command and Control", "Defense Evasion", "Initial Access"),
    confidence = c(0.9, 0.8, 0.7)
  )

  expect_s3_class(plot_top_tools(visualization_tools), "ggplot")
  expect_s3_class(plot_confidence_distribution(visualization_tools), "ggplot")
  expect_s3_class(plot_tools_by_source(visualization_tools), "ggplot")
  expect_s3_class(plot_mitre_heatmap(visualization_matrix), "ggplot")
  expect_s3_class(plot_tactic_distribution(visualization_matrix), "ggplot")
})