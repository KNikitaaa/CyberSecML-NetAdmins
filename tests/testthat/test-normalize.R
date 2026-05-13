test_that("normalize_raw_data builds canonical rows from all sources", {
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "acme",
    repo = "toolkit",
    full_name = "acme/toolkit",
    html_url = "https://github.com/acme/toolkit",
    api_url = "https://api.github.com/repos/acme/toolkit",
    description = "Offensive toolkit",
    language = "R",
    topics = list(c("red-team")),
    stargazers_count = 42L,
    forks_count = 3L,
    open_issues_count = 0L,
    archived = FALSE,
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = NA_character_,
    created_at = "2026-04-01T00:00:00Z",
    updated_at = "2026-04-02T00:00:00Z",
    pushed_at = "2026-04-03T00:00:00Z",
    readme = "README body",
    matched_queries = list(c("pentest tool")),
    matched_query_text = "pentest tool",
    search_score = 10.5
  )

  packetstorm_raw <- tibble::tibble(
    source = "packetstorm",
    source_type = "feed_item",
    category = "tool",
    title = "Packet Tool",
    url = "https://packetstorm.example/tool",
    guid = "packet-1",
    description = "Packet description",
    pub_date = "Sat, 04 Apr 2026 12:00:00 GMT",
    feed_url = "https://packetstorm.example/feed.xml",
    raw_content = "Packet description"
  )

  rss_raw <- tibble::tibble(
    source = "exploit_db",
    source_type = "rss_item",
    feed_title = "Exploit DB",
    feed_url = "https://www.exploit-db.com/rss.xml",
    item_title = "RSS Item",
    item_link = "https://example.com/rss-item",
    item_description = "RSS description",
    item_pub_date = "Sat, 04 Apr 2026 12:00:00 GMT",
    item_guid = "rss-1",
    item_categories = list(c("tag-a")),
    raw_content = "RSS description"
  )

  output_path <- tempfile(fileext = ".rds")
  normalized <- normalize_raw_data(
    raw_list = list(
      github = github_raw,
      packetstorm = packetstorm_raw,
      rss = rss_raw
    ),
    output_path = output_path
  )

  expect_equal(nrow(normalized), 3)
  expect_true(file.exists(output_path))
  expect_setequal(
    names(normalized),
    c(
      "record_id",
      "name",
      "source",
      "source_type",
      "url",
      "raw_description",
      "raw_text",
      "date_found",
      "pre_llm_score",
      "pre_llm_priority",
      "pre_llm_candidate_type",
      "pre_llm_should_process",
      "pre_llm_reasons",
      "metadata"
    )
  )

  expect_true(all(normalized$pre_llm_score >= 0))
  expect_true(all(normalized$pre_llm_score <= 100))
  expect_true(all(normalized$pre_llm_priority %in% c("low", "medium", "high")))
  expect_true(all(normalized$pre_llm_candidate_type %in% c("utility", "utility_suite", "script_collection", "fresh_discovery", "news_or_advisory", "non_tool")))
  expect_type(normalized$pre_llm_should_process, "logical")
})

test_that("normalize_raw_data deduplicates by URL", {
  github_raw <- tibble::tibble(
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    owner = c("acme", "acme"),
    repo = c("toolkit", "toolkit"),
    full_name = c("acme/toolkit", "acme/toolkit"),
    html_url = c("https://github.com/acme/toolkit", "https://github.com/acme/toolkit"),
    api_url = c("https://api.github.com/repos/acme/toolkit", "https://api.github.com/repos/acme/toolkit"),
    description = c("One", "Two"),
    language = c("R", "R"),
    topics = list(c("red-team"), c("red-team")),
    stargazers_count = c(42L, 42L),
    forks_count = c(1L, 1L),
    open_issues_count = c(0L, 0L),
    archived = c(FALSE, FALSE),
    default_branch = c("main", "main"),
    license_key = c("mit", "mit"),
    license_name = c("MIT License", "MIT License"),
    homepage = c(NA_character_, NA_character_),
    created_at = c("2026-04-01T00:00:00Z", "2026-04-01T00:00:00Z"),
    updated_at = c("2026-04-02T00:00:00Z", "2026-04-02T00:00:00Z"),
    pushed_at = c("2026-04-03T00:00:00Z", "2026-04-03T00:00:00Z"),
    readme = c("README 1", "README 2"),
    matched_queries = list(c("pentest tool"), c("red team tool")),
    matched_query_text = c("pentest tool", "red team tool"),
    search_score = c(11.1, 9.9)
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_equal(nrow(normalized), 1)
  expect_true(normalized$pre_llm_score[[1]] >= 0)
})

test_that("normalize_raw_data truncates oversized raw_text", {
  long_text <- paste(rep("abc", 5000), collapse = "")
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "acme",
    repo = "toolkit",
    full_name = "acme/toolkit",
    html_url = "https://github.com/acme/toolkit",
    api_url = "https://api.github.com/repos/acme/toolkit",
    description = long_text,
    language = "R",
    topics = list(c("red-team")),
    stargazers_count = 42L,
    forks_count = 1L,
    open_issues_count = 0L,
    archived = FALSE,
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = NA_character_,
    created_at = "2026-04-01T00:00:00Z",
    updated_at = "2026-04-02T00:00:00Z",
    pushed_at = "2026-04-03T00:00:00Z",
    readme = NA_character_,
    matched_queries = list(c("pentest tool")),
    matched_query_text = "pentest tool",
    search_score = 10.5
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_lte(nchar(normalized$raw_text[[1]], type = "chars"), 4000)
})

test_that("normalize_raw_data cleans markdown and code noise from README text", {
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "acme",
    repo = "toolkit",
    full_name = "acme/toolkit",
    html_url = "https://github.com/acme/toolkit",
    api_url = "https://api.github.com/repos/acme/toolkit",
    description = "Offensive toolkit",
    language = "R",
    topics = list(c("red-team")),
    stargazers_count = 42L,
    forks_count = 1L,
    open_issues_count = 0L,
    archived = FALSE,
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = NA_character_,
    created_at = "2026-04-01T00:00:00Z",
    updated_at = "2026-04-02T00:00:00Z",
    pushed_at = "2026-04-03T00:00:00Z",
    readme = paste(
      "# Toolkit",
      "![badge](https://img.shields.io/example)",
      "Use [docs](https://example.com/docs)",
      "```bash install.sh ```",
      sep = "\n"
    ),
    matched_queries = list(c("pentest tool")),
    matched_query_text = "pentest tool",
    matched_search_modes = list(c("stars", "updated")),
    matched_search_mode_text = "stars; updated",
    search_score = 10.5
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_false(grepl("img.shields.io", normalized$raw_text[[1]], fixed = TRUE))
  expect_false(grepl("install.sh", normalized$raw_text[[1]], fixed = TRUE))
  expect_match(normalized$raw_text[[1]], "Use docs", fixed = TRUE)
})

test_that("normalize_raw_data boosts seeded offensive tools", {
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "bishopfox",
    repo = "sliver",
    full_name = "bishopfox/sliver",
    html_url = "https://github.com/bishopfox/sliver",
    api_url = "https://api.github.com/repos/bishopfox/sliver",
    description = "Adversary emulation framework and red team command and control platform",
    language = "Go",
    topics = list(c("red-team", "c2")),
    stargazers_count = 7000L,
    forks_count = 500L,
    open_issues_count = 10L,
    archived = FALSE,
    default_branch = "main",
    license_key = "gpl-3.0",
    license_name = "GNU GPL v3",
    homepage = NA_character_,
    created_at = "2026-04-01T00:00:00Z",
    updated_at = "2026-04-02T00:00:00Z",
    pushed_at = "2026-04-03T00:00:00Z",
    readme = "Command and control framework for adversary simulation and post-exploitation.",
    matched_queries = list(c("red team tool", '"sliver" in:name,description')),
    matched_query_text = 'red team tool; "sliver" in:name,description',
    search_score = 15.0
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_gte(normalized$pre_llm_score[[1]], 70)
  expect_equal(normalized$pre_llm_priority[[1]], "high")
  expect_equal(normalized$pre_llm_candidate_type[[1]], "utility_suite")
  expect_true(normalized$pre_llm_should_process[[1]])
  expect_true(length(normalized$pre_llm_reasons[[1]]) > 0)
})

test_that("normalize_raw_data excludes vulnerability news from LLM queue", {
  rss_raw <- tibble::tibble(
    source = "cisa_advisories",
    source_type = "rss_item",
    feed_title = "CISA Advisories",
    feed_url = "https://www.cisa.gov/cybersecurity-advisories/all.xml",
    item_title = "CVE-2026-9999 Critical vulnerability in WidgetOS",
    item_link = "https://www.cisa.gov/news-events/alerts/2026/04/04/example",
    item_description = "Security advisory with mitigation guidance for a critical vulnerability.",
    item_pub_date = "Sat, 04 Apr 2026 12:00:00 GMT",
    item_guid = "cisa-1",
    item_categories = list(c("advisory", "cve")),
    raw_content = "Security advisory with mitigation guidance for a critical vulnerability."
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = NULL, packetstorm = NULL, rss = rss_raw),
    output_path = tempfile(fileext = ".rds")
  )

  expect_equal(normalized$pre_llm_candidate_type[[1]], "news_or_advisory")
  expect_false(normalized$pre_llm_should_process[[1]])
  expect_equal(normalized$pre_llm_score[[1]], 0)
  expect_equal(normalized$pre_llm_priority[[1]], "low")
})

test_that("normalize_raw_data excludes cheatsheets and exam-prep repositories from LLM queue", {
  github_raw <- tibble::tibble(
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    owner = c("anon-exploiter", "0xsyr0"),
    repo = c("sliver-cheatsheet", "OSCP"),
    full_name = c("anon-exploiter/sliver-cheatsheet", "0xsyr0/OSCP"),
    html_url = c("https://github.com/anon-exploiter/sliver-cheatsheet", "https://github.com/0xsyr0/OSCP"),
    api_url = c("https://api.github.com/repos/anon-exploiter/sliver-cheatsheet", "https://api.github.com/repos/0xsyr0/OSCP"),
    description = c(
      "Cheat sheet for Sliver commands and OSEP preparation.",
      "OSCP exam notes and pentest preparation references."
    ),
    language = c("Markdown", "Markdown"),
    topics = list(c("red-team", "cheatsheet"), c("oscp", "notes")),
    stargazers_count = c(100L, 200L),
    forks_count = c(10L, 20L),
    open_issues_count = c(0L, 0L),
    archived = c(FALSE, FALSE),
    default_branch = c("main", "main"),
    license_key = c("mit", "mit"),
    license_name = c("MIT License", "MIT License"),
    homepage = c(NA_character_, NA_character_),
    created_at = c("2026-04-01T00:00:00Z", "2026-04-01T00:00:00Z"),
    updated_at = c("2026-04-02T00:00:00Z", "2026-04-02T00:00:00Z"),
    pushed_at = c("2026-04-03T00:00:00Z", "2026-04-03T00:00:00Z"),
    readme = c(
      "Cheat sheet for Sliver, Impacket, and other commands.",
      "Study notes and exam preparation references for OSCP."
    ),
    matched_queries = list(c("red team tool"), c("pentest tool")),
    matched_query_text = c("red team tool", "pentest tool"),
    search_score = c(9.1, 8.8)
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_true(all(!normalized$pre_llm_should_process))
  expect_true(all(normalized$pre_llm_candidate_type == "non_tool"))
  expect_true(all(vapply(normalized$pre_llm_reasons, function(reasons) identical(reasons, "excluded:documentation_like"), logical(1))))
})

test_that("normalize_raw_data excludes markdown-only helper repositories without executable signals", {
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "acme",
    repo = "ad-helpers",
    full_name = "acme/ad-helpers",
    html_url = "https://github.com/acme/ad-helpers",
    api_url = "https://api.github.com/repos/acme/ad-helpers",
    description = "Helpers for Active Directory enumeration workflows.",
    language = "Markdown",
    topics = list(c("active-directory", "helpers")),
    stargazers_count = 30L,
    forks_count = 4L,
    open_issues_count = 0L,
    archived = FALSE,
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = NA_character_,
    created_at = "2026-04-01T00:00:00Z",
    updated_at = "2026-04-02T00:00:00Z",
    pushed_at = "2026-04-03T00:00:00Z",
    readme = "Helpers and operator notes for AD enumeration and pentest workflows.",
    matched_queries = list(c("pentest tool")),
    matched_query_text = "pentest tool",
    search_score = 6.2
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_false(normalized$pre_llm_should_process[[1]])
  expect_equal(normalized$pre_llm_candidate_type[[1]], "non_tool")
  expect_equal(normalized$pre_llm_reasons[[1]], "excluded:repository_without_executable_artifacts")
})

test_that("normalize_raw_data keeps script repositories with executable signals", {
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "acme",
    repo = "ops-scripts",
    full_name = "acme/ops-scripts",
    html_url = "https://github.com/acme/ops-scripts",
    api_url = "https://api.github.com/repos/acme/ops-scripts",
    description = "Collection of PowerShell scripts for AD enumeration and lateral movement.",
    language = "PowerShell",
    topics = list(c("active-directory", "powershell", "red-team")),
    stargazers_count = 75L,
    forks_count = 8L,
    open_issues_count = 0L,
    archived = FALSE,
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = NA_character_,
    created_at = "2026-04-01T00:00:00Z",
    updated_at = "2026-04-02T00:00:00Z",
    pushed_at = "2026-04-03T00:00:00Z",
    readme = "PowerShell scripts for enumeration. Quick start: run the script from PowerShell.",
    matched_queries = list(c("pentest tool")),
    matched_query_text = "pentest tool",
    search_score = 7.5
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_true(normalized$pre_llm_should_process[[1]])
  expect_equal(normalized$pre_llm_candidate_type[[1]], "script_collection")
})

test_that("normalize_raw_data sends fresh low-star discovery repositories to LLM", {
  github_raw <- tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = "acme",
    repo = "new-lab",
    full_name = "acme/new-lab",
    html_url = "https://github.com/acme/new-lab",
    api_url = "https://api.github.com/repos/acme/new-lab",
    description = "Small early release",
    language = "Go",
    topics = list(c("research")),
    stargazers_count = 0L,
    forks_count = 0L,
    open_issues_count = 0L,
    archived = FALSE,
    default_branch = "main",
    license_key = "mit",
    license_name = "MIT License",
    homepage = NA_character_,
    created_at = format(Sys.Date() - 7L, "%Y-%m-%dT00:00:00Z"),
    updated_at = format(Sys.Date() - 6L, "%Y-%m-%dT00:00:00Z"),
    pushed_at = format(Sys.Date() - 5L, "%Y-%m-%dT00:00:00Z"),
    readme = "Early release.",
    matched_queries = list(c("created:recent")),
    matched_query_families = list(c("recent_discovery")),
    matched_query_tiers = list(c("broad")),
    matched_query_text = "created:recent",
    matched_search_modes = list(c("updated")),
    matched_search_mode_text = "updated",
    search_score = 2.4
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  expect_true(normalized$pre_llm_should_process[[1]])
  expect_equal(normalized$pre_llm_candidate_type[[1]], "fresh_discovery")
  expect_true("candidate:fresh_discovery" %in% normalized$pre_llm_reasons[[1]])
  expect_true(any(grepl("fresh_discovery_lane", normalized$pre_llm_reasons[[1]], fixed = TRUE)))
})

test_that("normalize_raw_data prioritizes utilities above loose script collections", {
  github_raw <- tibble::tibble(
    source = c("github", "github"),
    source_type = c("repository", "repository"),
    owner = c("bishopfox", "acme"),
    repo = c("sliver", "ad-scripts"),
    full_name = c("bishopfox/sliver", "acme/ad-scripts"),
    html_url = c("https://github.com/bishopfox/sliver", "https://github.com/acme/ad-scripts"),
    api_url = c("https://api.github.com/repos/bishopfox/sliver", "https://api.github.com/repos/acme/ad-scripts"),
    description = c(
      "Adversary emulation framework and command and control platform",
      "Collection of pentest scripts for Active Directory enumeration"
    ),
    language = c("Go", "Python"),
    topics = list(c("red-team", "c2"), c("scripts", "active-directory")),
    stargazers_count = c(7000L, 40L),
    forks_count = c(500L, 5L),
    open_issues_count = c(10L, 0L),
    archived = c(FALSE, FALSE),
    default_branch = c("main", "main"),
    license_key = c("gpl-3.0", "mit"),
    license_name = c("GNU GPL v3", "MIT License"),
    homepage = c(NA_character_, NA_character_),
    created_at = c("2026-04-01T00:00:00Z", "2026-04-01T00:00:00Z"),
    updated_at = c("2026-04-02T00:00:00Z", "2026-04-02T00:00:00Z"),
    pushed_at = c("2026-04-03T00:00:00Z", "2026-04-03T00:00:00Z"),
    readme = c(
      "Command and control framework for adversary simulation.",
      "Helper scripts for AD enumeration and pentest workflows."
    ),
    matched_queries = list(
      c("red team tool", '"sliver" in:name,description'),
      c("pentest tool")
    ),
    matched_query_text = c('red team tool; "sliver" in:name,description', "pentest tool"),
    search_score = c(15.0, 7.0)
  )

  normalized <- normalize_raw_data(
    raw_list = list(github = github_raw, packetstorm = NULL, rss = NULL),
    output_path = tempfile(fileext = ".rds")
  )

  sliver_row <- normalized[normalized$name == "sliver", , drop = FALSE]
  script_row <- normalized[normalized$name == "ad-scripts", , drop = FALSE]

  expect_equal(sliver_row$pre_llm_candidate_type[[1]], "utility_suite")
  expect_equal(script_row$pre_llm_candidate_type[[1]], "script_collection")
  expect_true(sliver_row$pre_llm_should_process[[1]])
  expect_true(script_row$pre_llm_should_process[[1]])
  expect_gt(sliver_row$pre_llm_score[[1]], script_row$pre_llm_score[[1]])
})
