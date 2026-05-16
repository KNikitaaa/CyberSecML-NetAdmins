test_that("validation enrichment schema exposes required fields", {
  schema <- get_validation_enrichment_schema()

  expect_equal(schema$type, "object")
  expect_true("is_tool" %in% schema$required)
  expect_true("capabilities" %in% names(schema$properties))
})

test_that("parse_validation_enrichment_json validates enrichment payloads", {
  payload <- paste0(
    '{',
    '"is_tool": true,',
    '"name": "Toolkit",',
    '"purpose": "Offensive testing",',
    '"capabilities": ["phishing", "recon"],',
    '"target_platforms": ["Windows", "Linux"],',
    '"category": "framework",',
    '"reason": "Matches offensive-tool behavior"',
    '}'
  )

  result <- parse_validation_enrichment_json(payload)

  expect_equal(nrow(result), 1)
  expect_true(result$is_tool[[1]])
  expect_equal(result$capabilities[[1]], c("phishing", "recon"))
})

test_that("parse_validation_enrichment_json rejects invalid booleans", {
  payload <- paste0(
    '{',
    '"is_tool": "maybe",',
    '"name": "Toolkit",',
    '"purpose": "Offensive testing",',
    '"capabilities": ["phishing"],',
    '"target_platforms": ["Windows"],',
    '"category": "framework",',
    '"reason": "Reason"',
    '}'
  )

  expect_error(parse_validation_enrichment_json(payload), "must be boolean")
})

test_that("mitre classification schema exposes classification array", {
  schema <- get_mitre_classification_schema()

  expect_equal(schema$type, "object")
  expect_true("classifications" %in% schema$required)
})

test_that("parse_mitre_classification_json validates technique mappings", {
  payload <- paste0(
    '{',
    '"tool_name": "Toolkit",',
    '"classifications": [',
    '{"technique_id": "T1566", "technique_name": "Phishing", "tactic": "Initial Access", "confidence": 0.91, "reasoning": "Capability matches phishing."}',
    ']',
    '}'
  )

  result <- parse_mitre_classification_json(payload)

  expect_equal(nrow(result), 1)
  expect_equal(result$tool_name[[1]], "Toolkit")
  expect_equal(result$technique_id[[1]], "T1566")
})

test_that("parse_mitre_classification_json rejects invalid confidence", {
  payload <- paste0(
    '{',
    '"tool_name": "Toolkit",',
    '"classifications": [',
    '{"technique_id": "T1566", "technique_name": "Phishing", "tactic": "Initial Access", "confidence": "high", "reasoning": "Reason"}',
    ']',
    '}'
  )

  expect_error(parse_mitre_classification_json(payload), "must be numeric")
})