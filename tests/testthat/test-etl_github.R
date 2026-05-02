expect_equal <- testthat::expect_equal
expect_match <- testthat::expect_match

test_that("get_default_github_queries returns curated defaults", {
  queries <- get_default_github_queries()
  seed_names <- get_default_github_seed_names()
  topics <- get_default_github_topics()
  sort_modes <- get_default_github_search_modes()

  expect_type(queries, "character")
  expect_true(length(queries) >= 20)
  expect_true(any(grepl("red team", queries, fixed = TRUE)))
  expect_true("sliver" %in% seed_names)
  expect_true(any(grepl('"metasploit"', queries, fixed = TRUE)))
  expect_true(any(grepl("topic:red-team", queries, fixed = TRUE)))
  expect_true("red-team" %in% topics)
  expect_true("recent_discovery" %in% get_default_github_fresh_discovery_families())
  expect_setequal(sort_modes, c("updated", "best-match"))
})

test_that("fetch_repo_details returns normalized repository metadata", {
  target_env <- environment(fetch_repo_details)
  original_request <- get(".github_request", envir = target_env, inherits = FALSE)
  binding_was_locked <- bindingIsLocked(".github_request", target_env)

  if (binding_was_locked) {
    unlockBinding(".github_request", target_env)
  }

  assign(
    ".github_request",
    function(endpoint, ...) {
      if (identical(endpoint, "/repos/{owner}/{repo}")) {
        return(list(
          name = "toolkit",
          full_name = "acme/toolkit",
          html_url = "https://github.com/acme/toolkit",
          url = "https://api.github.com/repos/acme/toolkit",
          description = "Offensive toolkit",
          language = "R",
          topics = list("red-team", "offensive-security"),
          stargazers_count = 42,
          forks_count = 7,
          open_issues_count = 2,
          archived = FALSE,
          default_branch = "main",
          homepage = "https://example.com/toolkit",
          created_at = "2025-01-01T00:00:00Z",
          updated_at = "2025-01-02T00:00:00Z",
          pushed_at = "2025-01-03T00:00:00Z",
          owner = list(login = "acme"),
          license = list(key = "mit", name = "MIT License")
        ))
      }

      if (identical(endpoint, "/repos/{owner}/{repo}/readme")) {
        return(list(
          encoding = "base64",
          content = jsonlite::base64_enc(charToRaw("# Toolkit\nExample README"))
        ))
      }

      stop("Unexpected endpoint", call. = FALSE)
    },
    envir = target_env
  )
  on.exit({
    assign(".github_request", original_request, envir = target_env)
    if (binding_was_locked) {
      lockBinding(".github_request", target_env)
    }
  }, add = TRUE)

  details <- fetch_repo_details("acme", "toolkit")

  expect_equal(nrow(details), 1)
  expect_equal(details$full_name[[1]], "acme/toolkit")
  expect_equal(details$stargazers_count[[1]], 42)
  expect_equal(details$topics[[1]], c("red-team", "offensive-security"))
  expect_match(details$readme[[1]], "Example README", fixed = TRUE)
})

test_that("search_github_tools deduplicates repositories across queries", {
  tracker <- new.env(parent = emptyenv())
  tracker$search_calls <- 0L
  target_env <- environment(search_github_tools)
  original_request <- get(".github_request", envir = target_env, inherits = FALSE)
  original_fetch_repo_details <- get("fetch_repo_details", envir = target_env, inherits = FALSE)
  request_binding_was_locked <- bindingIsLocked(".github_request", target_env)
  fetch_binding_was_locked <- bindingIsLocked("fetch_repo_details", target_env)

  if (request_binding_was_locked) {
    unlockBinding(".github_request", target_env)
  }

  if (fetch_binding_was_locked) {
    unlockBinding("fetch_repo_details", target_env)
  }

  assign(
    ".github_request",
    function(endpoint, q = NULL, ...) {
      if (!identical(endpoint, "/search/repositories")) {
        stop("Unexpected endpoint", call. = FALSE)
      }

      tracker$search_calls <- tracker$search_calls + 1L

      if (grepl("pentest tool", q, fixed = TRUE)) {
        return(list(
          items = list(
            list(
              name = "toolkit",
              full_name = "acme/toolkit",
              html_url = "https://github.com/acme/toolkit",
              description = "Toolkit",
              language = "R",
              archived = FALSE,
              stargazers_count = 42,
              score = 12.5,
              owner = list(login = "acme")
            )
          )
        ))
      }

      list(
        items = list(
          list(
            name = "toolkit",
            full_name = "acme/toolkit",
            html_url = "https://github.com/acme/toolkit",
            description = "Toolkit",
            language = "R",
            archived = FALSE,
            stargazers_count = 42,
            score = 9.5,
            owner = list(login = "acme")
          ),
          list(
            name = "operator",
            full_name = "acme/operator",
            html_url = "https://github.com/acme/operator",
            description = "Operator",
            language = "Go",
            archived = FALSE,
            stargazers_count = 21,
            score = 8.1,
            owner = list(login = "acme")
          )
        )
      )
    },
    envir = target_env
  )

  assign(
    "fetch_repo_details",
    function(owner, repo, include_readme = TRUE, token = NULL) {
      tibble::tibble(
        source = "github",
        source_type = "repository",
        owner = owner,
        repo = repo,
        full_name = sprintf("%s/%s", owner, repo),
        html_url = sprintf("https://github.com/%s/%s", owner, repo),
        api_url = sprintf("https://api.github.com/repos/%s/%s", owner, repo),
        description = sprintf("%s description", repo),
        language = if (identical(repo, "toolkit")) "R" else "Go",
        topics = list(c("offensive-security")),
        stargazers_count = if (identical(repo, "toolkit")) 42L else 21L,
        forks_count = 1L,
        open_issues_count = 0L,
        archived = FALSE,
        default_branch = "main",
        license_key = "mit",
        license_name = "MIT License",
        homepage = NA_character_,
        created_at = "2025-01-01T00:00:00Z",
        updated_at = "2025-01-02T00:00:00Z",
        pushed_at = "2025-01-03T00:00:00Z",
        readme = "README",
        matched_queries = list(character(0)),
        matched_query_text = NA_character_,
        search_score = NA_real_
      )
    },
    envir = target_env
  )

  on.exit({
    assign(".github_request", original_request, envir = target_env)
    if (request_binding_was_locked) {
      lockBinding(".github_request", target_env)
    }
  }, add = TRUE)
  on.exit({
    assign("fetch_repo_details", original_fetch_repo_details, envir = target_env)
    if (fetch_binding_was_locked) {
      lockBinding("fetch_repo_details", target_env)
    }
  }, add = TRUE)

  results <- search_github_tools(
    queries = c("pentest tool", "red team tool"),
    max_results = 5,
    include_readme = FALSE,
    sort_modes = "stars"
  )

  expect_equal(tracker$search_calls, 2L)
  expect_equal(nrow(results), 2)
  expect_equal(results$full_name[[1]], "acme/toolkit")
  expect_setequal(results$matched_queries[[1]], c("pentest tool", "red team tool"))
  expect_equal(results$matched_search_modes[[1]], "stars")
})

test_that("collect_github_tools writes the raw GitHub artifact", {
  output_path <- tempfile(fileext = ".rds")
  target_env <- environment(collect_github_tools)
  original_search <- get("search_github_tools", envir = target_env, inherits = FALSE)
  binding_was_locked <- bindingIsLocked("search_github_tools", target_env)

  if (binding_was_locked) {
    unlockBinding("search_github_tools", target_env)
  }

  assign(
    "search_github_tools",
    function(...) {
      tibble::tibble(
        source = "github",
        source_type = "repository",
        owner = "acme",
        repo = "toolkit",
        full_name = "acme/toolkit",
        html_url = "https://github.com/acme/toolkit",
        api_url = "https://api.github.com/repos/acme/toolkit",
        description = "Toolkit",
        language = "R",
        topics = list(c("red-team")),
        stargazers_count = 42L,
        forks_count = 7L,
        open_issues_count = 2L,
        archived = FALSE,
        default_branch = "main",
        license_key = "mit",
        license_name = "MIT License",
        homepage = NA_character_,
        created_at = "2025-01-01T00:00:00Z",
        updated_at = "2025-01-02T00:00:00Z",
        pushed_at = "2025-01-03T00:00:00Z",
        readme = "README",
        matched_queries = list(c("pentest tool")),
        matched_query_text = "pentest tool",
        search_score = 12.5
      )
    },
    envir = target_env
  )
  on.exit({
    assign("search_github_tools", original_search, envir = target_env)
    if (binding_was_locked) {
      lockBinding("search_github_tools", target_env)
    }
  }, add = TRUE)

  artifact <- collect_github_tools(output_path = output_path)

  expect_true(file.exists(output_path))
  expect_equal(nrow(artifact), 1)

  stored <- readRDS(output_path)
  expect_equal(stored$full_name[[1]], "acme/toolkit")
})

test_that("search_github_tools degrades gracefully when a query fails", {
  target_env <- environment(search_github_tools)
  original_request <- get(".github_request", envir = target_env, inherits = FALSE)
  original_fetch_repo_details <- get("fetch_repo_details", envir = target_env, inherits = FALSE)
  request_binding_was_locked <- bindingIsLocked(".github_request", target_env)
  fetch_binding_was_locked <- bindingIsLocked("fetch_repo_details", target_env)

  if (request_binding_was_locked) {
    unlockBinding(".github_request", target_env)
  }

  if (fetch_binding_was_locked) {
    unlockBinding("fetch_repo_details", target_env)
  }

  assign(
    ".github_request",
    function(endpoint, q = NULL, ...) {
      if (!identical(endpoint, "/search/repositories")) {
        stop("Unexpected endpoint", call. = FALSE)
      }

      if (grepl("broken query", q, fixed = TRUE)) {
        stop("rate limit", call. = FALSE)
      }

      list(
        items = list(
          list(
            name = "toolkit",
            full_name = "acme/toolkit",
            html_url = "https://github.com/acme/toolkit",
            description = "Toolkit",
            language = "R",
            archived = FALSE,
            stargazers_count = 42,
            score = 9.5,
            owner = list(login = "acme")
          )
        )
      )
    },
    envir = target_env
  )

  assign(
    "fetch_repo_details",
    function(owner, repo, include_readme = TRUE, token = NULL) {
      tibble::tibble(
        source = "github",
        source_type = "repository",
        owner = owner,
        repo = repo,
        full_name = sprintf("%s/%s", owner, repo),
        html_url = sprintf("https://github.com/%s/%s", owner, repo),
        api_url = sprintf("https://api.github.com/repos/%s/%s", owner, repo),
        description = sprintf("%s description", repo),
        language = "R",
        topics = list(c("offensive-security")),
        stargazers_count = 42L,
        forks_count = 1L,
        open_issues_count = 0L,
        archived = FALSE,
        default_branch = "main",
        license_key = "mit",
        license_name = "MIT License",
        homepage = NA_character_,
        created_at = "2025-01-01T00:00:00Z",
        updated_at = "2025-01-02T00:00:00Z",
        pushed_at = "2025-01-03T00:00:00Z",
        readme = "README",
        matched_queries = list(character(0)),
        matched_query_text = NA_character_,
        matched_search_modes = list(character(0)),
        matched_search_mode_text = NA_character_,
        search_score = NA_real_
      )
    },
    envir = target_env
  )

  on.exit({
    assign(".github_request", original_request, envir = target_env)
    if (request_binding_was_locked) {
      lockBinding(".github_request", target_env)
    }
  }, add = TRUE)
  on.exit({
    assign("fetch_repo_details", original_fetch_repo_details, envir = target_env)
    if (fetch_binding_was_locked) {
      lockBinding("fetch_repo_details", target_env)
    }
  }, add = TRUE)

  results <- search_github_tools(
    queries = c("broken query", "working query"),
    sort_modes = "stars",
    include_readme = FALSE,
    max_results = 5
  )

  expect_equal(nrow(results), 1)
  expect_equal(results$full_name[[1]], "acme/toolkit")
})

test_that("search_github_tools stops early when enough unique candidates are found", {
  target_env <- environment(search_github_tools)
  original_request <- get(".github_request", envir = target_env, inherits = FALSE)
  original_fetch_repo_details <- get("fetch_repo_details", envir = target_env, inherits = FALSE)
  request_binding_was_locked <- bindingIsLocked(".github_request", target_env)
  fetch_binding_was_locked <- bindingIsLocked("fetch_repo_details", target_env)
  tracker <- new.env(parent = emptyenv())
  tracker$request_count <- 0L

  if (request_binding_was_locked) {
    unlockBinding(".github_request", target_env)
  }

  if (fetch_binding_was_locked) {
    unlockBinding("fetch_repo_details", target_env)
  }

  assign(
    ".github_request",
    function(endpoint, q = NULL, ...) {
      if (!identical(endpoint, "/search/repositories")) {
        stop("Unexpected endpoint", call. = FALSE)
      }

      tracker$request_count <- tracker$request_count + 1L

      list(
        items = lapply(
          seq_len(6),
          function(index) {
            repo_name <- sprintf("repo-%s-%s", tracker$request_count, index)

            list(
              name = repo_name,
              full_name = sprintf("acme/%s", repo_name),
              html_url = sprintf("https://github.com/acme/%s", repo_name),
              description = "Toolkit",
              language = "R",
              archived = FALSE,
              stargazers_count = 50 - index,
              score = 10 - (index / 10),
              owner = list(login = "acme")
            )
          }
        )
      )
    },
    envir = target_env
  )

  assign(
    "fetch_repo_details",
    function(owner, repo, include_readme = TRUE, token = NULL) {
      tibble::tibble(
        source = "github",
        source_type = "repository",
        owner = owner,
        repo = repo,
        full_name = sprintf("%s/%s", owner, repo),
        html_url = sprintf("https://github.com/%s/%s", owner, repo),
        api_url = sprintf("https://api.github.com/repos/%s/%s", owner, repo),
        description = sprintf("%s description", repo),
        language = "R",
        topics = list(c("offensive-security")),
        stargazers_count = 42L,
        forks_count = 1L,
        open_issues_count = 0L,
        archived = FALSE,
        default_branch = "main",
        license_key = "mit",
        license_name = "MIT License",
        homepage = NA_character_,
        created_at = "2025-01-01T00:00:00Z",
        updated_at = "2025-01-02T00:00:00Z",
        pushed_at = "2025-01-03T00:00:00Z",
        readme = "README",
        matched_queries = list(character(0)),
        matched_query_text = NA_character_,
        matched_search_modes = list(character(0)),
        matched_search_mode_text = NA_character_,
        search_score = NA_real_
      )
    },
    envir = target_env
  )

  on.exit({
    assign(".github_request", original_request, envir = target_env)
    if (request_binding_was_locked) {
      lockBinding(".github_request", target_env)
    }
  }, add = TRUE)
  on.exit({
    assign("fetch_repo_details", original_fetch_repo_details, envir = target_env)
    if (fetch_binding_was_locked) {
      lockBinding("fetch_repo_details", target_env)
    }
  }, add = TRUE)

  results <- search_github_tools(
    queries = c("query-a", "query-b", "query-c", "query-d"),
    sort_modes = c("stars", "updated"),
    max_results = 5,
    include_readme = FALSE,
    max_search_requests = 20,
    min_search_requests = 2
  )

  expect_true(request_count < 8)
  expect_equal(nrow(results), 5)
})

test_that("search_github_tools runs fresh discovery families before early stop", {
  target_env <- environment(search_github_tools)
  original_request <- get(".github_request", envir = target_env, inherits = FALSE)
  original_fetch_repo_details <- get("fetch_repo_details", envir = target_env, inherits = FALSE)
  request_binding_was_locked <- bindingIsLocked(".github_request", target_env)
  fetch_binding_was_locked <- bindingIsLocked("fetch_repo_details", target_env)
  tracker <- new.env(parent = emptyenv())
  tracker$request_count <- 0L
  tracker$queries <- character(0)

  if (request_binding_was_locked) {
    unlockBinding(".github_request", target_env)
  }

  if (fetch_binding_was_locked) {
    unlockBinding("fetch_repo_details", target_env)
  }

  assign(
    ".github_request",
    function(endpoint, q = NULL, ...) {
      if (!identical(endpoint, "/search/repositories")) {
        stop("Unexpected endpoint", call. = FALSE)
      }

      tracker$request_count <- tracker$request_count + 1L
      tracker$queries <- c(tracker$queries, q)

      list(
        items = lapply(
          seq_len(3),
          function(index) {
            repo_name <- sprintf("repo-%s-%s", tracker$request_count, index)

            list(
              name = repo_name,
              full_name = sprintf("acme/%s", repo_name),
              html_url = sprintf("https://github.com/acme/%s", repo_name),
              description = "Fresh offensive toolkit",
              language = "Go",
              archived = FALSE,
              stargazers_count = 0L,
              score = 10 - index,
              created_at = sprintf("2025-01-%02dT00:00:00Z", index),
              updated_at = sprintf("2025-01-%02dT00:00:00Z", index),
              pushed_at = sprintf("2025-01-%02dT00:00:00Z", index),
              owner = list(login = "acme")
            )
          }
        )
      )
    },
    envir = target_env
  )

  assign(
    "fetch_repo_details",
    function(owner, repo, include_readme = TRUE, token = NULL) {
      tibble::tibble(
        source = "github",
        source_type = "repository",
        owner = owner,
        repo = repo,
        full_name = sprintf("%s/%s", owner, repo),
        html_url = sprintf("https://github.com/%s/%s", owner, repo),
        api_url = sprintf("https://api.github.com/repos/%s/%s", owner, repo),
        description = sprintf("%s description", repo),
        language = "Go",
        topics = list(c("offensive-security")),
        stargazers_count = 0L,
        forks_count = 0L,
        open_issues_count = 0L,
        archived = FALSE,
        default_branch = "main",
        license_key = "mit",
        license_name = "MIT License",
        homepage = NA_character_,
        created_at = "2025-01-03T00:00:00Z",
        updated_at = "2025-01-03T00:00:00Z",
        pushed_at = "2025-01-03T00:00:00Z",
        readme = "README",
        matched_queries = list(character(0)),
        matched_query_text = NA_character_,
        matched_search_modes = list(character(0)),
        matched_search_mode_text = NA_character_,
        search_score = NA_real_
      )
    },
    envir = target_env
  )

  on.exit({
    assign(".github_request", original_request, envir = target_env)
    if (request_binding_was_locked) {
      lockBinding(".github_request", target_env)
    }
  }, add = TRUE)
  on.exit({
    assign("fetch_repo_details", original_fetch_repo_details, envir = target_env)
    if (fetch_binding_was_locked) {
      lockBinding("fetch_repo_details", target_env)
    }
  }, add = TRUE)

  query_specs <- tibble::tibble(
    query = c("precision one", "precision two", "fresh discovery"),
    query_family = c("precision_keywords", "precision_keywords", "recent_discovery"),
    query_tier = c("precision", "precision", "broad"),
    query_min_stars = 0L,
    query_sort_modes = list("updated", "updated", "updated"),
    query_pages = list(1L, 1L, 1L)
  )

  results <- search_github_tools(
    queries = query_specs,
    max_results = 1,
    include_readme = FALSE,
    max_search_requests = 10,
    min_search_requests = 1
  )

  expect_true(tracker$request_count >= 2L)
  expect_true(any(grepl("fresh discovery", tracker$queries, fixed = TRUE)))
  expect_equal(nrow(results), 1)
})
