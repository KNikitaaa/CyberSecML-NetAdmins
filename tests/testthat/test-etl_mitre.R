make_test_stix_bundle <- function() {
  list(
    type = "bundle",
    id = "bundle--test",
    objects = list(
      list(
        type = "x-mitre-tactic",
        id = "x-mitre-tactic--001",
        name = "Initial Access",
        description = "Adversaries try to get into your network.",
        x_mitre_shortname = "initial-access",
        created = "2024-01-01T00:00:00.000Z",
        modified = "2024-01-02T00:00:00.000Z",
        external_references = list(
          list(source_name = "mitre-attack", external_id = "TA0001")
        )
      ),
      list(
        type = "attack-pattern",
        id = "attack-pattern--001",
        name = "Phishing",
        description = "Send a phishing message to gain access.",
        x_mitre_is_subtechnique = FALSE,
        x_mitre_platforms = list("Windows", "Linux"),
        created = "2024-01-01T00:00:00.000Z",
        modified = "2024-01-02T00:00:00.000Z",
        kill_chain_phases = list(
          list(kill_chain_name = "mitre-attack", phase_name = "initial-access")
        ),
        external_references = list(
          list(source_name = "mitre-attack", external_id = "T1566")
        )
      ),
      list(
        type = "attack-pattern",
        id = "attack-pattern--002",
        name = "Spearphishing Link",
        description = "Send a phishing link to gain access.",
        x_mitre_is_subtechnique = TRUE,
        x_mitre_platforms = list("Windows"),
        created = "2024-01-01T00:00:00.000Z",
        modified = "2024-01-02T00:00:00.000Z",
        kill_chain_phases = list(
          list(kill_chain_name = "mitre-attack", phase_name = "initial-access")
        ),
        external_references = list(
          list(source_name = "mitre-attack", external_id = "T1566.002")
        )
      ),
      list(
        type = "attack-pattern",
        id = "attack-pattern--003",
        name = "Deprecated Technique",
        description = "Deprecated technique should be excluded.",
        x_mitre_deprecated = TRUE,
        created = "2024-01-01T00:00:00.000Z",
        modified = "2024-01-02T00:00:00.000Z",
        external_references = list(
          list(source_name = "mitre-attack", external_id = "T0000")
        )
      )
    )
  )
}

test_that("parse_mitre_tactics returns normalized tactic rows", {
  tactics <- parse_mitre_tactics(make_test_stix_bundle())

  expect_equal(nrow(tactics), 1)
  expect_equal(tactics$tactic_id[[1]], "TA0001")
  expect_equal(tactics$tactic_shortname[[1]], "initial-access")
  expect_equal(tactics$tactic_name[[1]], "Initial Access")
})

test_that("parse_mitre_techniques excludes deprecated rows and preserves sub-techniques", {
  techniques <- parse_mitre_techniques(make_test_stix_bundle())

  expect_equal(nrow(techniques), 2)
  expect_setequal(techniques$technique_id, c("T1566", "T1566.002"))
  expect_true(any(techniques$is_subtechnique))
  expect_true(all(techniques$tactic_shortname == "initial-access"))
})

test_that("get_mitre_matrix joins tactic metadata onto techniques", {
  mitre_matrix <- get_mitre_matrix(make_test_stix_bundle())

  expect_equal(nrow(mitre_matrix), 2)
  expect_true(all(mitre_matrix$tactic_name == "Initial Access"))
  expect_true(all(!is.na(mitre_matrix$tactic_id)))
})

test_that("save_mitre_attack_dataset writes an rda artifact", {
  output_path <- tempfile(fileext = ".rda")

  save_mitre_attack_dataset(
    stix_data = make_test_stix_bundle(),
    output_path = output_path
  )

  expect_true(file.exists(output_path))

  load(output_path)
  expect_true(exists("mitre_attack"))
  expect_equal(nrow(mitre_attack), 2)
})