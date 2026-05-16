test_that("search_tools filters the visualization layer", {
  data_dir <- tempfile(pattern = "mcp-data-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  tools <- tibble::tibble(
    record_id = c("tool-1", "tool-2"),
    assessed_name = c("Sliver", "Ligolo-ng"),
    source = c("github", "packetstorm"),
    source_type = c("repository", "advisory"),
    url = c("https://example.com/sliver", "https://example.com/ligolo"),
    date_found = c("2026-04-04", "2026-04-04"),
    entity_type = c("framework", "utility"),
    category_ru = c("C2", "Pivoting"),
    short_description_ru = c("Фреймворк для red team операций", "Туннель для pivoting"),
    long_description_ru = c("Длинное описание Sliver", "Длинное описание Ligolo"),
    summary_ru = c("summary 1", "summary 2"),
    purpose_ru = c("purpose 1", "purpose 2"),
    capabilities_ru = list(c("c2"), c("pivot")),
    reason_ru = c("reason 1", "reason 2"),
    pre_llm_score = c(95, 80),
    pre_llm_priority = c("high", "high"),
    confidence_score = c(0.96, 0.84),
    detail_score = c(0.90, 0.70),
    mitre_score = c(0.80, 0.55),
    entity_priority_score = c(1.0, 0.9),
    visualization_score = c(0.93, 0.79),
    overall_confidence = c(0.96, 0.84),
    llm_provider = c("deepseek", "deepseek"),
    llm_model = c("deepseek-chat", "deepseek-chat"),
    mitre_tactics = list(c("Command and Control"), c("Command and Control", "Lateral Movement")),
    mitre_technique_ids = list(c("T1071"), c("T1572")),
    mitre_technique_names = list(c("Application Layer Protocol"), c("Protocol Tunneling")),
    mitre_tactic_count = c(1L, 2L),
    mitre_technique_count = c(1L, 1L),
    filter_tags = list(c("source:github"), c("source:packetstorm", "mitre:tactic:lateral_movement")),
    visualization_rank = c(1L, 2L),
    mitre_matrix = list(list(), list())
  )

  matrix <- tibble::tibble(
    record_id = c("tool-1", "tool-2"),
    assessed_name = c("Sliver", "Ligolo-ng"),
    source = c("github", "packetstorm"),
    url = c("https://example.com/sliver", "https://example.com/ligolo"),
    entity_type = c("framework", "utility"),
    category_ru = c("C2", "Pivoting"),
    technique_id = c("T1071", "T1572"),
    technique_name = c("Application Layer Protocol", "Protocol Tunneling"),
    tactic = c("Command and Control", "Lateral Movement"),
    confidence = c(0.96, 0.84),
    reasoning_ru = c("reason 1", "reason 2"),
    tactic_tag = c("mitre:tactic:command_and_control", "mitre:tactic:lateral_movement"),
    technique_tag = c("mitre:technique:T1071", "mitre:technique:T1572")
  )

  saveRDS(tools, file.path(data_dir, "visualization_tools.rds"))
  saveRDS(matrix, file.path(data_dir, "visualization_tool_matrix.rds"))

  result <- search_tools(query = "pivot", data_dir = data_dir)
  expect_equal(nrow(result), 1)
  expect_equal(result$assessed_name[[1]], "Ligolo-ng")

  source_filtered <- search_tools(source = "github", data_dir = data_dir)
  expect_equal(nrow(source_filtered), 1)
  expect_equal(source_filtered$assessed_name[[1]], "Sliver")

  tactic_filtered <- search_tools(tactic = "Lateral Movement", data_dir = data_dir)
  expect_equal(nrow(tactic_filtered), 1)
  expect_equal(tactic_filtered$record_id[[1]], "tool-2")
})

test_that("tool and technique MCP helpers expose linked MITRE data", {
  data_dir <- tempfile(pattern = "mcp-data-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  tools <- tibble::tibble(
    record_id = c("tool-1", "tool-2"),
    assessed_name = c("Sliver", "Ligolo-ng"),
    source = c("github", "packetstorm"),
    source_type = c("repository", "advisory"),
    url = c("https://example.com/sliver", "https://example.com/ligolo"),
    date_found = c("2026-04-04", "2026-04-04"),
    entity_type = c("framework", "utility"),
    category_ru = c("C2", "Pivoting"),
    short_description_ru = c("Фреймворк для red team операций", "Туннель для pivoting"),
    long_description_ru = c("Длинное описание Sliver", "Длинное описание Ligolo"),
    summary_ru = c("summary 1", "summary 2"),
    purpose_ru = c("purpose 1", "purpose 2"),
    capabilities_ru = list(c("c2"), c("pivot")),
    reason_ru = c("reason 1", "reason 2"),
    pre_llm_score = c(95, 80),
    pre_llm_priority = c("high", "high"),
    confidence_score = c(0.96, 0.84),
    detail_score = c(0.90, 0.70),
    mitre_score = c(0.80, 0.55),
    entity_priority_score = c(1.0, 0.9),
    visualization_score = c(0.93, 0.79),
    overall_confidence = c(0.96, 0.84),
    llm_provider = c("deepseek", "deepseek"),
    llm_model = c("deepseek-chat", "deepseek-chat"),
    mitre_tactics = list(c("Command and Control"), c("Lateral Movement")),
    mitre_technique_ids = list(c("T1071"), c("T1572")),
    mitre_technique_names = list(c("Application Layer Protocol"), c("Protocol Tunneling")),
    mitre_tactic_count = c(1L, 1L),
    mitre_technique_count = c(1L, 1L),
    filter_tags = list(c("source:github"), c("source:packetstorm")),
    visualization_rank = c(1L, 2L),
    mitre_matrix = list(list(), list())
  )

  matrix <- tibble::tibble(
    record_id = c("tool-1", "tool-2"),
    assessed_name = c("Sliver", "Ligolo-ng"),
    source = c("github", "packetstorm"),
    url = c("https://example.com/sliver", "https://example.com/ligolo"),
    entity_type = c("framework", "utility"),
    category_ru = c("C2", "Pivoting"),
    technique_id = c("T1071", "T1572"),
    technique_name = c("Application Layer Protocol", "Protocol Tunneling"),
    tactic = c("Command and Control", "Lateral Movement"),
    confidence = c(0.96, 0.84),
    reasoning_ru = c("reason 1", "reason 2"),
    tactic_tag = c("mitre:tactic:command_and_control", "mitre:tactic:lateral_movement"),
    technique_tag = c("mitre:technique:T1071", "mitre:technique:T1572")
  )

  saveRDS(tools, file.path(data_dir, "visualization_tools.rds"))
  saveRDS(matrix, file.path(data_dir, "visualization_tool_matrix.rds"))

  tool_ttps <- get_tool_ttps(tool_name = "sliver", data_dir = data_dir)
  expect_equal(tool_ttps$tool$record_id[[1]], "tool-1")
  expect_equal(nrow(tool_ttps$mitre_mappings), 1)
  expect_equal(tool_ttps$mitre_mappings$technique_id[[1]], "T1071")

  technique_tools <- get_technique_tools("T1572", data_dir = data_dir)
  expect_equal(nrow(technique_tools), 1)
  expect_equal(technique_tools$assessed_name[[1]], "Ligolo-ng")

  stats <- get_statistics(data_dir = data_dir)
  expect_equal(stats$tool_count, 2)
  expect_equal(stats$technique_count, 2)
  expect_true("source_breakdown" %in% names(stats))
})

test_that("MCP helpers tolerate an empty fresh-clone data directory", {
  data_dir <- tempfile(pattern = "mcp-empty-data-")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  result <- search_tools(data_dir = data_dir)
  expect_equal(nrow(result), 0)

  stats <- get_statistics(data_dir = data_dir)
  expect_equal(stats$tool_count, 0)
  expect_equal(stats$mapping_count, 0)
  expect_equal(nrow(stats$source_breakdown), 0)
  expect_equal(names(stats$source_breakdown), c("source", "count"))
})
