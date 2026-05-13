.normalize_empty_results <- function() {
  tibble::tibble(
    record_id = character(),
    name = character(),
    source = character(),
    source_type = character(),
    url = character(),
    raw_description = character(),
    raw_text = character(),
    date_found = character(),
    pre_llm_score = numeric(),
    pre_llm_priority = character(),
    pre_llm_candidate_type = character(),
    pre_llm_should_process = logical(),
    pre_llm_reasons = list(),
    metadata = list()
  )
}

.normalize_seed_names <- function() {
  c(
    "metasploit",
    "sliver",
    "ligolo-ng",
    "impacket",
    "bloodhound",
    "responder",
    "crackmapexec",
    "netexec",
    "sqlmap",
    "nuclei",
    "evil-winrm",
    "chisel",
    "mythic",
    "havoc",
    "merlin",
    "empire",
    "powerview",
    "rubeus",
    "seatbelt",
    "sharpsploit",
    "pacu",
    "linpeas",
    "winpeas",
    "weevely",
    "pwncat",
    "frp",
    "fscan",
    "ffuf",
    "dirsearch",
    "feroxbuster",
    "wfuzz",
    "commix",
    "mimikatz",
    "kerbrute",
    "soaphound",
    "lsassy",
    "gobuster",
    "naabu",
    "subfinder",
    "httpx",
    "deimosc2",
    "viper",
    "tempest"
  )
}

.normalize_priority_keywords <- function() {
  c(
    "offensive",
    "red team",
    "pentest",
    "post-exploitation",
    "lateral movement",
    "credential",
    "phishing",
    "c2",
    "command and control",
    "adversary simulation",
    "osint",
    "reconnaissance",
    "exploit",
    "payload",
    "pivot",
    "framework",
    "toolkit",
    "operator",
    "automation",
    "implant",
    "agent",
    "enumeration",
    "discovery",
    "tunneling",
    "persistence",
    "evasion"
  )
}

.normalize_utility_keywords <- function() {
  c(
    "tool",
    "toolkit",
    "framework",
    "suite",
    "utility",
    "utilities",
    "agent",
    "implant",
    "beacon",
    "orchestrator",
    "automation",
    "operator",
    "cli",
    "scanner",
    "fuzzer",
    "payload",
    "listener",
    "loader",
    "proxy",
    "tunnel",
    "pivot",
    "enumeration",
    "enum",
    "bruteforce",
    "spray",
    "recon",
    "reconnaissance",
    "c2",
    "command and control",
    "post exploitation",
    "credential access",
    "persistence",
    "evasion",
    "collection",
    "exfiltration"
  )
}

.normalize_script_collection_keywords <- function() {
  c(
    "script",
    "scripts",
    "script collection",
    "collection of scripts",
    "arsenal",
    "helpers"
  )
}

.normalize_manual_identity_keywords <- function() {
  c(
    "cheatsheet",
    "cheat sheet",
    "cheat-sheet",
    "crib sheet",
    "cribsheet",
    "playbook",
    "playbooks",
    "manual",
    "guide",
    "guides",
    "reference",
    "references",
    "resources",
    "resource list",
    "knowledge base",
    "handbook",
    "cookbook",
    "checklist",
    "walkthrough",
    "walk through",
    "writeup",
    "write-up",
    "notes",
    "lab notes",
    "labs",
    "course",
    "coursework",
    "curriculum",
    "syllabus",
    "roadmap",
    "mindmap",
    "methodology",
    "awesome",
    "awesome list",
    "study guide",
    "study notes",
    "exam prep",
    "exam preparation",
    "cert prep",
    "certification prep",
    "oscp",
    "osep",
    "osed",
    "oswe",
    "osce",
    "oswa",
    "pnpt",
    "ejpt",
    "crto",
    "crtp",
    "crte",
    "cpts"
  )
}

.normalize_manual_context_keywords <- function() {
  c(
    "certification",
    "course notes",
    "study material",
    "training notes",
    "training material",
    "exam notes",
    "lab guide",
    "reference guide",
    "command reference",
    "preparation for exam",
    "prep for exam"
  )
}

.normalize_documentation_languages <- function() {
  c(
    "markdown",
    "mdx",
    "asciidoc",
    "rst",
    "text",
    "plain text",
    "tex"
  )
}

.normalize_repository_runtime_keywords <- function() {
  c(
    "cli",
    "command line",
    "binary",
    "server",
    "client",
    "agent",
    "implant",
    "beacon",
    "payload",
    "loader",
    "shellcode",
    "build",
    "compile",
    "compiled",
    "module",
    "plugin"
  )
}

.normalize_repository_packaging_keywords <- function() {
  c(
    "pip install",
    "go build",
    "go install",
    "cargo build",
    "cargo install",
    "npm install",
    "docker run",
    "make build",
    "requirements txt",
    "pyproject toml",
    "setup py",
    "install sh",
    "quick start",
    "getting started"
  )
}

.normalize_news_keywords <- function() {
  c(
    "cve-",
    "vulnerability",
    "vulnerabilities",
    "advisory",
    "bulletin",
    "security update",
    "patch tuesday",
    "zero-day",
    "zero day",
    "mitigation",
    "workaround"
  )
}

.normalize_detect_keywords <- function(text, keywords) {
  if (is.na(text) || !nzchar(text) || length(keywords) == 0) {
    return(character(0))
  }

  normalized_text <- tolower(text)
  normalized_text <- gsub("[^a-z0-9]+", " ", normalized_text)
  normalized_text <- stringr::str_squish(normalized_text)
  normalized_text <- paste0(" ", normalized_text, " ")

  keywords[
    vapply(
      keywords,
      function(keyword) {
        normalized_keyword <- tolower(keyword)
        normalized_keyword <- gsub("[^a-z0-9]+", " ", normalized_keyword)
        normalized_keyword <- stringr::str_squish(normalized_keyword)

        if (!nzchar(normalized_keyword)) {
          return(FALSE)
        }

        stringr::str_detect(
          normalized_text,
          stringr::fixed(paste0(" ", normalized_keyword, " "))
        )
      },
      logical(1)
    )
  ]
}

.normalize_metadata_text <- function(metadata) {
  metadata <- .normalize_value_or(metadata, list())

  components <- c(
    .normalize_string(metadata$feed_title),
    .normalize_string(metadata$category),
    .normalize_string(metadata$credit),
    .normalize_string(metadata$matched_query_text),
    paste(.normalize_value_or(metadata$topics, character(0)), collapse = " "),
    paste(.normalize_value_or(metadata$item_categories, character(0)), collapse = " "),
    paste(.normalize_value_or(metadata$tag_names, character(0)), collapse = " ")
  )

  components <- components[!is.na(components) & nzchar(components)]

  if (length(components) == 0) {
    return("")
  }

  stringr::str_squish(paste(components, collapse = " "))
}

.normalize_is_documentation_like_candidate <- function(name, raw_text = NA_character_, metadata = list(), url = NA_character_) {
  metadata_text <- .normalize_metadata_text(metadata)
  identity_text <- stringr::str_squish(paste(
    dplyr::coalesce(.normalize_string(name), ""),
    dplyr::coalesce(.normalize_string(url), ""),
    metadata_text,
    collapse = " "
  ))
  searchable_text <- stringr::str_squish(paste(
    identity_text,
    dplyr::coalesce(.normalize_string(raw_text), ""),
    collapse = " "
  ))

  matched_identity_keywords <- .normalize_detect_keywords(identity_text, .normalize_manual_identity_keywords())
  matched_context_keywords <- .normalize_detect_keywords(searchable_text, .normalize_manual_context_keywords())

  length(matched_identity_keywords) > 0 || length(matched_context_keywords) > 0
}

.normalize_is_documentation_language <- function(language) {
  normalized_language <- tolower(dplyr::coalesce(.normalize_string(language), ""))

  nzchar(normalized_language) && normalized_language %in% .normalize_documentation_languages()
}

.normalize_has_executable_repository_signals <- function(name, raw_text = NA_character_, metadata = list(), url = NA_character_) {
  metadata <- .normalize_value_or(metadata, list())
  language <- .normalize_string(metadata$language)
  searchable_text <- stringr::str_squish(paste(
    dplyr::coalesce(.normalize_string(name), ""),
    dplyr::coalesce(.normalize_string(url), ""),
    dplyr::coalesce(.normalize_string(raw_text), ""),
    dplyr::coalesce(.normalize_string(metadata$matched_query_text), ""),
    collapse = " "
  ))

  matched_runtime_keywords <- .normalize_detect_keywords(searchable_text, .normalize_repository_runtime_keywords())
  matched_packaging_keywords <- .normalize_detect_keywords(searchable_text, .normalize_repository_packaging_keywords())

  has_non_documentation_language <- !is.na(language) && nzchar(language) && !.normalize_is_documentation_language(language)

  has_non_documentation_language ||
    length(matched_runtime_keywords) > 0 ||
    length(matched_packaging_keywords) > 0
}

.normalize_classify_candidate <- function(source, source_type, name, raw_text, metadata, url = NA_character_) {
  source <- .normalize_string(source)
  source_type <- .normalize_string(source_type)
  name <- dplyr::coalesce(.normalize_string(name), "")
  raw_text <- dplyr::coalesce(.normalize_string(raw_text), "")
  url <- dplyr::coalesce(.normalize_string(url), "")
  metadata_text <- .normalize_metadata_text(metadata)
  searchable_text <- stringr::str_squish(paste(name, raw_text, metadata_text, url, collapse = " "))
  documentation_like <- .normalize_is_documentation_like_candidate(
    name = name,
    raw_text = raw_text,
    metadata = metadata,
    url = url
  )

  matched_seed_names <- .normalize_detect_keywords(searchable_text, .normalize_seed_names())
  matched_utility_keywords <- .normalize_detect_keywords(searchable_text, .normalize_utility_keywords())
  matched_script_keywords <- .normalize_detect_keywords(searchable_text, .normalize_script_collection_keywords())
  matched_news_keywords <- .normalize_detect_keywords(searchable_text, .normalize_news_keywords())
  matched_query_families <- .normalize_value_or(metadata$matched_query_families, character(0))

  packetstorm_tool <- identical(source, "packetstorm") && (
    identical(.normalize_string(metadata$category), "tool") ||
      length(matched_seed_names) > 0 ||
      length(matched_utility_keywords) > 0
  )
  github_repository <- identical(source, "github") && identical(source_type, "repository")
  executable_repository <- if (github_repository) {
    .normalize_has_executable_repository_signals(
      name = name,
      raw_text = raw_text,
      metadata = metadata,
      url = url
    )
  } else {
    TRUE
  }
  fresh_discovery <- github_repository && .normalize_is_fresh_github_discovery(metadata)
  utility_query_signal <- any(matched_query_families %in% c("precision_keywords", "balanced_keywords", "seed_names", "org_publishers"))
  strong_tool_signal <- length(matched_seed_names) > 0 || length(matched_utility_keywords) > 0 || utility_query_signal || packetstorm_tool
  script_collection <- length(matched_script_keywords) > 0
  news_like <- identical(source, "cisa_advisories") || (length(matched_news_keywords) > 0 && !strong_tool_signal)

  if (news_like) {
    return(list(
      candidate_type = "news_or_advisory",
      should_process = FALSE,
      score_bonus = 0,
      reasons = c(sprintf("excluded:news(%s)", paste(matched_news_keywords, collapse = ",")))
    ))
  }

  if (documentation_like && !packetstorm_tool) {
    return(list(
      candidate_type = "non_tool",
      should_process = FALSE,
      score_bonus = 0,
      reasons = "excluded:documentation_like"
    ))
  }

  if (github_repository && !executable_repository) {
    return(list(
      candidate_type = "non_tool",
      should_process = FALSE,
      score_bonus = 0,
      reasons = "excluded:repository_without_executable_artifacts"
    ))
  }

  if (github_repository && !strong_tool_signal && !script_collection) {
    if (fresh_discovery) {
      return(list(
        candidate_type = "fresh_discovery",
        should_process = TRUE,
        score_bonus = 8,
        reasons = "candidate:fresh_discovery"
      ))
    }

    return(list(
      candidate_type = "non_tool",
      should_process = FALSE,
      score_bonus = 0,
      reasons = "excluded:github_repository_without_utility_signal"
    ))
  }

  if (script_collection && (length(matched_seed_names) > 0 || strong_tool_signal)) {
    return(list(
      candidate_type = "script_collection",
      should_process = TRUE,
      score_bonus = 10,
      reasons = c(sprintf("candidate:script_collection(%s)", paste(matched_script_keywords, collapse = ",")))
    ))
  }

  if (length(matched_seed_names) > 0) {
    return(list(
      candidate_type = "utility_suite",
      should_process = TRUE,
      score_bonus = 24,
      reasons = c(sprintf("candidate:seeded_utility(%s)", paste(matched_seed_names, collapse = ",")))
    ))
  }

  if (strong_tool_signal) {
    return(list(
      candidate_type = "utility",
      should_process = TRUE,
      score_bonus = 18,
      reasons = c(sprintf("candidate:utility(%s)", paste(matched_utility_keywords, collapse = ",")))
    ))
  }

  list(
    candidate_type = "non_tool",
    should_process = FALSE,
    score_bonus = 0,
    reasons = "excluded:non_tool"
  )
}

.normalize_value_or <- function(value, default) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  value
}

.normalize_string <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || is.na(first_value)) {
    return(NA_character_)
  }

  first_value <- as.character(first_value)
  first_value <- stringr::str_squish(first_value)

  if (!nzchar(first_value)) {
    return(NA_character_)
  }

  first_value
}

.normalize_text_block <- function(..., max_chars = 4000L) {
  values <- list(...)
  values <- lapply(values, .normalize_string)
  values <- Filter(function(value) !is.na(value) && nzchar(value), values)

  if (length(values) == 0) {
    return(NA_character_)
  }

  text <- paste(unlist(values, use.names = FALSE), collapse = "\n\n")

  .clean_candidate_text(text, max_chars = max_chars)
}

.normalize_canonicalize_url <- function(url) {
  normalized <- .normalize_string(url)

  if (is.na(normalized) || !nzchar(normalized)) {
    return(NA_character_)
  }

  normalized <- tolower(normalized)
  normalized <- sub("^https?://api[.]github[.]com/repos/", "https://github.com/", normalized)
  normalized <- sub("[?#].*$", "", normalized)
  normalized <- sub("[.]git$", "", normalized)
  normalized <- sub("/+$", "", normalized)
  stringr::str_squish(normalized)
}

.normalize_canonicalize_name <- function(name) {
  normalized <- .normalize_string(name)

  if (is.na(normalized) || !nzchar(normalized)) {
    return(NA_character_)
  }

  normalized <- tolower(normalized)
  normalized <- gsub("[^a-z0-9]+", " ", normalized)
  stringr::str_squish(normalized)
}

.normalize_metadata_scalar <- function(metadata, field_names) {
  metadata <- .normalize_value_or(metadata, list())

  for (field_name in field_names) {
    if (!(field_name %in% names(metadata))) {
      next
    }

    value <- .normalize_string(metadata[[field_name]])
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }

  NA_character_
}

.normalize_row_value <- function(row, field_name, default = NULL) {
  if (!is.data.frame(row) || !(field_name %in% names(row))) {
    return(default)
  }

  row[[field_name]]
}

.normalize_source_identity <- function(source, url, name, metadata = list()) {
  source_part <- dplyr::coalesce(.normalize_string(source), "unknown")
  canonical_url <- .normalize_canonicalize_url(url)
  canonical_name <- .normalize_canonicalize_name(name)
  metadata <- .normalize_value_or(metadata, list())

  identity_body <- switch(
    source_part,
    github = .normalize_metadata_scalar(metadata, c("full_name", "repo", "canonical_url")),
    packetstorm = .normalize_metadata_scalar(metadata, c("guid", "canonical_url")),
    rss = .normalize_metadata_scalar(metadata, c("item_guid", "canonical_url")),
    cisa_advisories = .normalize_metadata_scalar(metadata, c("item_guid", "canonical_url")),
    .normalize_metadata_scalar(metadata, c("guid", "item_guid", "canonical_url"))
  )

  if (is.na(identity_body) || !nzchar(identity_body)) {
    identity_body <- canonical_url
  }

  if (is.na(identity_body) || !nzchar(identity_body)) {
    identity_body <- canonical_name
  }

  if (is.na(identity_body) || !nzchar(identity_body)) {
    return(paste(source_part, "row", sep = "::"))
  }

  paste(source_part, tolower(identity_body), sep = "::")
}

.normalize_make_record_id <- function(source, url, name, metadata = list()) {
  .normalize_source_identity(source = source, url = url, name = name, metadata = metadata)
}

.normalize_extract_metadata_field <- function(metadata_column, field_name) {
  if (!is.list(metadata_column) || length(metadata_column) == 0) {
    return(character(0))
  }

  vapply(metadata_column, function(entry) {
    if (!is.list(entry) || !(field_name %in% names(entry))) {
      return(NA_character_)
    }

    .normalize_string(entry[[field_name]])
  }, character(1))
}

.normalize_row_completeness <- function(normalized_df) {
  if (!is.data.frame(normalized_df) || nrow(normalized_df) == 0) {
    return(numeric(0))
  }

  raw_text_length <- nchar(dplyr::coalesce(as.character(normalized_df[["raw_text"]]), ""))
  description_length <- nchar(dplyr::coalesce(as.character(normalized_df[["raw_description"]]), ""))
  url_present <- as.integer(!is.na(normalized_df[["url"]]) & nzchar(as.character(normalized_df[["url"]])))
  date_present <- as.integer(!is.na(normalized_df[["date_found"]]) & nzchar(as.character(normalized_df[["date_found"]])))
  metadata_count <- if ("metadata" %in% names(normalized_df) && is.list(normalized_df[["metadata"]])) {
    vapply(normalized_df[["metadata"]], function(entry) {
      if (!is.list(entry) || length(entry) == 0) {
        return(0)
      }

      values <- unlist(entry, recursive = FALSE, use.names = FALSE)
      sum(vapply(values, function(value) {
        if (length(value) == 0 || is.null(value)) {
          return(FALSE)
        }

        if (length(value) > 1) {
          value <- paste(as.character(value), collapse = " ")
        }

        normalized_value <- .normalize_string(value)
        !is.na(normalized_value) && nzchar(normalized_value)
      }, logical(1)))
    }, integer(1))
  } else {
    integer(nrow(normalized_df))
  }

  as.numeric(url_present * 20L) +
    pmin(raw_text_length, 600L) / 12 +
    pmin(description_length, 240L) / 20 +
    as.numeric(date_present * 5L) +
    pmin(metadata_count, 12L) * 2
}

.normalize_priority_bucket <- function(score) {
  if (is.na(score)) {
    return("low")
  }

  if (score >= 70) {
    return("high")
  }

  if (score >= 40) {
    return("medium")
  }

  "low"
}

.normalize_parse_date <- function(value) {
  normalized_value <- .normalize_string(value)

  if (is.na(normalized_value)) {
    return(as.Date(NA))
  }

  tryCatch(
    suppressWarnings(as.Date(substr(normalized_value, 1L, 10L))),
    error = function(error) as.Date(NA)
  )
}

.normalize_is_fresh_github_discovery <- function(metadata, max_age_days = 180L) {
  metadata <- .normalize_value_or(metadata, list())
  matched_query_families <- .normalize_value_or(metadata$matched_query_families, character(0))
  matched_query_tiers <- .normalize_value_or(metadata$matched_query_tiers, character(0))
  matched_search_modes <- .normalize_value_or(metadata$matched_search_modes, character(0))
  fresh_families <- c(
    "recent_discovery",
    "rolling_windows",
    "topic_discovery",
    "topic_language",
    "star_band_discovery",
    "language_shards",
    "name_fragment_discovery"
  )

  discovery_signal <- any(matched_query_families %in% fresh_families) ||
    "broad" %in% matched_query_tiers ||
    "updated" %in% matched_search_modes

  created_date <- .normalize_parse_date(metadata$created_at)
  pushed_date <- .normalize_parse_date(metadata$pushed_at)
  updated_date <- .normalize_parse_date(metadata$updated_at)
  candidate_dates <- c(created_date, pushed_date, updated_date)
  candidate_dates <- candidate_dates[!is.na(candidate_dates)]

  if (!isTRUE(discovery_signal) || length(candidate_dates) == 0) {
    return(FALSE)
  }

  age_days <- min(as.integer(Sys.Date() - candidate_dates), na.rm = TRUE)
  is.finite(age_days) && age_days >= 0L && age_days <= as.integer(max_age_days)
}

.normalize_compute_priority <- function(
  source,
  source_type,
  name,
  raw_text,
  metadata,
  url = NA_character_
) {
  score <- 0
  reasons <- character(0)
  source <- .normalize_string(source)
  source_type <- .normalize_string(source_type)
  name <- dplyr::coalesce(.normalize_string(name), "")
  raw_text <- dplyr::coalesce(.normalize_string(raw_text), "")
  url <- dplyr::coalesce(.normalize_string(url), "")
  metadata <- .normalize_value_or(metadata, list())
  searchable_text <- stringr::str_squish(paste(name, raw_text, url, collapse = " "))

  classification <- .normalize_classify_candidate(
    source = source,
    source_type = source_type,
    name = name,
    raw_text = raw_text,
    metadata = metadata,
    url = url
  )
  reasons <- c(reasons, classification$reasons)

  if (!isTRUE(classification$should_process)) {
    return(list(
      score = 0,
      priority = "low",
      candidate_type = classification$candidate_type,
      should_process = FALSE,
      reasons = unique(reasons)
    ))
  }

  score <- score + classification$score_bonus

  source_bonus <- switch(
    source,
    github = 20,
    packetstorm = 18,
    exploit_db = 14,
    cisa_advisories = 8,
    10
  )
  score <- score + source_bonus
  reasons <- c(reasons, sprintf("source:%s(+%s)", source, source_bonus))

  keywords <- .normalize_priority_keywords()
  matched_keywords <- keywords[
    vapply(
      keywords,
      function(keyword) stringr::str_detect(searchable_text, stringr::fixed(tolower(keyword))),
      logical(1)
    )
  ]

  if (length(matched_keywords) > 0) {
    keyword_bonus <- min(20, length(matched_keywords) * 4)
    score <- score + keyword_bonus
    reasons <- c(reasons, sprintf("keywords:%s(+%s)", paste(matched_keywords, collapse = ","), keyword_bonus))
  }

  seed_names <- .normalize_seed_names()
  matched_seed_names <- seed_names[
    vapply(
      seed_names,
      function(seed_name) stringr::str_detect(searchable_text, stringr::fixed(tolower(seed_name))),
      logical(1)
    )
  ]

  if (length(matched_seed_names) > 0) {
    seed_bonus <- min(25, 12 + (length(matched_seed_names) - 1L) * 4L)
    score <- score + seed_bonus
    reasons <- c(reasons, sprintf("seed:%s(+%s)", paste(matched_seed_names, collapse = ","), seed_bonus))
  }

  if (identical(source, "github")) {
    stars <- suppressWarnings(as.numeric(.normalize_value_or(metadata$stargazers_count, NA_real_)))
    search_score <- suppressWarnings(as.numeric(.normalize_value_or(metadata$search_score, NA_real_)))
    matched_queries <- .normalize_value_or(metadata$matched_queries, character(0))
    matched_query_families <- .normalize_value_or(metadata$matched_query_families, character(0))
    matched_query_tiers <- .normalize_value_or(metadata$matched_query_tiers, character(0))
    matched_search_modes <- .normalize_value_or(metadata$matched_search_modes, character(0))
    created_date <- .normalize_parse_date(metadata$created_at)
    updated_date <- .normalize_parse_date(metadata$updated_at)
    archived <- isTRUE(.normalize_value_or(metadata$archived, FALSE))

    if (!is.na(stars)) {
      stars_bonus <- min(25, floor(log10(stars + 1) * 10))
      score <- score + stars_bonus
      reasons <- c(reasons, sprintf("stars:%s(+%s)", as.integer(stars), stars_bonus))
    }

    if (!is.na(search_score)) {
      search_bonus <- min(15, floor(search_score))
      score <- score + search_bonus
      reasons <- c(reasons, sprintf("search_score:%s(+%s)", round(search_score, 2), search_bonus))
    }

    if (length(matched_queries) > 0) {
      query_bonus <- min(15, length(matched_queries) * 3)
      score <- score + query_bonus
      reasons <- c(reasons, sprintf("matched_queries:%s(+%s)", length(matched_queries), query_bonus))
    }

    if ("precision" %in% matched_query_tiers) {
      score <- score + 6
      reasons <- c(reasons, "query_tier:precision(+6)")
    } else if ("balanced" %in% matched_query_tiers) {
      score <- score + 3
      reasons <- c(reasons, "query_tier:balanced(+3)")
    }

    if (any(matched_query_families %in% c("precision_keywords", "seed_names", "org_publishers"))) {
      score <- score + 4
      reasons <- c(reasons, "query_family:utility_signal(+4)")
    }

    if ("updated" %in% matched_search_modes) {
      score <- score + 4
      reasons <- c(reasons, "search_mode:updated(+4)")
    }

    if (identical(classification$candidate_type, "fresh_discovery")) {
      score <- score + 10
      reasons <- c(reasons, "fresh_discovery_lane(+10)")
    }

    if (!is.na(created_date)) {
      created_age_days <- as.integer(Sys.Date() - created_date)
      created_bonus <- dplyr::case_when(
        created_age_days <= 30L ~ 10,
        created_age_days <= 90L ~ 7,
        created_age_days <= 180L ~ 4,
        .default = 0
      )

      if (created_bonus > 0) {
        score <- score + created_bonus
        reasons <- c(reasons, sprintf("recent_creation:%s_days(+%s)", created_age_days, created_bonus))
      }
    }

    if (!is.na(updated_date)) {
      age_days <- as.integer(Sys.Date() - updated_date)
      recency_bonus <- dplyr::case_when(
        age_days <= 30L ~ 10,
        age_days <= 90L ~ 6,
        age_days <= 180L ~ 3,
        .default = 0
      )

      if (recency_bonus > 0) {
        score <- score + recency_bonus
        reasons <- c(reasons, sprintf("recent_update:%s_days(+%s)", age_days, recency_bonus))
      }
    }

    if (archived) {
      score <- score - 20
      reasons <- c(reasons, "archived(-20)")
    }
  }

  score <- max(0, min(100, score))

  list(
    score = as.numeric(score),
    priority = .normalize_priority_bucket(score),
    candidate_type = classification$candidate_type,
    should_process = classification$should_process,
    reasons = unique(reasons)
  )
}

.normalize_github <- function(github_raw) {
  if (is.null(github_raw) || nrow(github_raw) == 0) {
    return(.normalize_empty_results())
  }

  rows <- lapply(
    seq_len(nrow(github_raw)),
    function(index) {
      row <- github_raw[index, , drop = FALSE]
      name <- .normalize_string(row$repo)
      url <- .normalize_string(row$html_url)
      raw_description <- .normalize_string(row$description)
      raw_text <- .normalize_text_block(row$description, row$readme)
      date_found <- dplyr::coalesce(.normalize_string(row$updated_at), .normalize_string(row$pushed_at))
      metadata <- list(
        owner = .normalize_string(row$owner),
        full_name = .normalize_string(row$full_name),
        api_url = .normalize_string(row$api_url),
        language = .normalize_string(row$language),
        topics = row$topics[[1]],
        stargazers_count = row$stargazers_count[[1]],
        forks_count = row$forks_count[[1]],
        open_issues_count = row$open_issues_count[[1]],
        archived = row$archived[[1]],
        default_branch = .normalize_string(row$default_branch),
        license_key = .normalize_string(row$license_key),
        license_name = .normalize_string(row$license_name),
        homepage = .normalize_string(row$homepage),
        created_at = .normalize_string(row$created_at),
        matched_queries = row$matched_queries[[1]],
        matched_query_families = if ("matched_query_families" %in% names(row)) row$matched_query_families[[1]] else character(0),
        matched_query_tiers = if ("matched_query_tiers" %in% names(row)) row$matched_query_tiers[[1]] else character(0),
        matched_query_text = .normalize_string(row$matched_query_text),
        matched_query_family_text = if ("matched_query_family_text" %in% names(row)) .normalize_string(row$matched_query_family_text) else NA_character_,
        matched_query_tier_text = if ("matched_query_tier_text" %in% names(row)) .normalize_string(row$matched_query_tier_text) else NA_character_,
        matched_search_modes = if ("matched_search_modes" %in% names(row)) row$matched_search_modes[[1]] else character(0),
        matched_search_mode_text = if ("matched_search_mode_text" %in% names(row)) .normalize_string(row$matched_search_mode_text) else NA_character_,
        updated_at = .normalize_string(row$updated_at),
        pushed_at = .normalize_string(row$pushed_at),
        search_score = row$search_score[[1]]
      )
      metadata$canonical_url <- .normalize_canonicalize_url(url)
      metadata$canonical_name <- .normalize_canonicalize_name(name)
      metadata$source_identity <- .normalize_source_identity("github", url, name, metadata)
      priority <- .normalize_compute_priority(
        source = "github",
        source_type = .normalize_string(row$source_type),
        name = name,
        raw_text = raw_text,
        metadata = metadata,
        url = url
      )

      tibble::tibble(
        record_id = .normalize_make_record_id("github", url, name, metadata),
        name = name,
        source = "github",
        source_type = .normalize_string(row$source_type),
        url = url,
        raw_description = raw_description,
        raw_text = raw_text,
        date_found = date_found,
        pre_llm_score = priority$score,
        pre_llm_priority = priority$priority,
        pre_llm_candidate_type = priority$candidate_type,
        pre_llm_should_process = priority$should_process,
        pre_llm_reasons = list(priority$reasons),
        metadata = list(metadata)
      )
    }
  )

  dplyr::bind_rows(rows)
}

.normalize_packetstorm <- function(packetstorm_raw) {
  if (is.null(packetstorm_raw) || nrow(packetstorm_raw) == 0) {
    return(.normalize_empty_results())
  }

  rows <- lapply(
    seq_len(nrow(packetstorm_raw)),
    function(index) {
      row <- packetstorm_raw[index, , drop = FALSE]
      name <- .normalize_string(row$title)
      url <- .normalize_string(row$url)
      raw_description <- .normalize_string(row$description)
      raw_text <- .normalize_text_block(row$title, row$description, row$raw_content)
      metadata <- list(
        category = .normalize_string(row$category),
        guid = .normalize_string(row$guid),
        feed_url = .normalize_string(row$feed_url),
        credit = .normalize_string(.normalize_row_value(row, "credit")),
        credit_url = .normalize_string(.normalize_row_value(row, "credit_url")),
        api_query = .normalize_string(.normalize_row_value(row, "api_query")),
        tag_names = .normalize_value_or(.normalize_row_value(row, "tag_names"), character(0))
      )
      metadata$canonical_url <- .normalize_canonicalize_url(url)
      metadata$canonical_name <- .normalize_canonicalize_name(name)
      metadata$source_identity <- .normalize_source_identity("packetstorm", url, name, metadata)
      priority <- .normalize_compute_priority(
        source = "packetstorm",
        source_type = .normalize_string(row$source_type),
        name = name,
        raw_text = raw_text,
        metadata = metadata,
        url = url
      )

      tibble::tibble(
        record_id = .normalize_make_record_id("packetstorm", url, name, metadata),
        name = name,
        source = "packetstorm",
        source_type = .normalize_string(row$source_type),
        url = url,
        raw_description = raw_description,
        raw_text = raw_text,
        date_found = .normalize_string(row$pub_date),
        pre_llm_score = priority$score,
        pre_llm_priority = priority$priority,
        pre_llm_candidate_type = priority$candidate_type,
        pre_llm_should_process = priority$should_process,
        pre_llm_reasons = list(priority$reasons),
        metadata = list(metadata)
      )
    }
  )

  dplyr::bind_rows(rows)
}

.normalize_rss <- function(rss_raw) {
  if (is.null(rss_raw) || nrow(rss_raw) == 0) {
    return(.normalize_empty_results())
  }

  rows <- lapply(
    seq_len(nrow(rss_raw)),
    function(index) {
      row <- rss_raw[index, , drop = FALSE]
      name <- .normalize_string(row$item_title)
      url <- .normalize_string(row$item_link)
      raw_description <- .normalize_string(row$item_description)
      raw_text <- .normalize_text_block(row$item_title, row$item_description, row$raw_content)
      metadata <- list(
        feed_title = .normalize_string(row$feed_title),
        feed_url = .normalize_string(row$feed_url),
        item_guid = .normalize_string(row$item_guid),
        item_categories = row$item_categories[[1]]
      )
      metadata$canonical_url <- .normalize_canonicalize_url(url)
      metadata$canonical_name <- .normalize_canonicalize_name(name)
      metadata$source_identity <- .normalize_source_identity(.normalize_string(row$source), url, name, metadata)
      priority <- .normalize_compute_priority(
        source = .normalize_string(row$source),
        source_type = .normalize_string(row$source_type),
        name = name,
        raw_text = raw_text,
        metadata = metadata,
        url = url
      )

      tibble::tibble(
        record_id = .normalize_make_record_id(.normalize_string(row$source), url, name, metadata),
        name = name,
        source = .normalize_string(row$source),
        source_type = .normalize_string(row$source_type),
        url = url,
        raw_description = raw_description,
        raw_text = raw_text,
        date_found = .normalize_string(row$item_pub_date),
        pre_llm_score = priority$score,
        pre_llm_priority = priority$priority,
        pre_llm_candidate_type = priority$candidate_type,
        pre_llm_should_process = priority$should_process,
        pre_llm_reasons = list(priority$reasons),
        metadata = list(metadata)
      )
    }
  )

  dplyr::bind_rows(rows)
}

.normalize_deduplicate <- function(normalized_df) {
  if (nrow(normalized_df) == 0) {
    return(normalized_df)
  }

  metadata_column <- if ("metadata" %in% names(normalized_df)) normalized_df[["metadata"]] else vector("list", nrow(normalized_df))
  canonical_url <- .normalize_extract_metadata_field(metadata_column, "canonical_url")
  source_identity <- .normalize_extract_metadata_field(metadata_column, "source_identity")
  canonical_name <- .normalize_extract_metadata_field(metadata_column, "canonical_name")

  if (length(canonical_url) != nrow(normalized_df)) {
    canonical_url <- rep(NA_character_, nrow(normalized_df))
  }

  if (length(source_identity) != nrow(normalized_df)) {
    source_identity <- rep(NA_character_, nrow(normalized_df))
  }

  if (length(canonical_name) != nrow(normalized_df)) {
    canonical_name <- rep(NA_character_, nrow(normalized_df))
  }

  missing_url <- is.na(canonical_url) | !nzchar(canonical_url)
  canonical_url[missing_url] <- vapply(normalized_df[["url"]], .normalize_canonicalize_url, character(1))

  missing_name <- is.na(canonical_name) | !nzchar(canonical_name)
  canonical_name[missing_name] <- vapply(normalized_df[["name"]], .normalize_canonicalize_name, character(1))

  missing_identity <- is.na(source_identity) | !nzchar(source_identity)
  source_identity[missing_identity] <- vapply(which(missing_identity), function(index) {
    .normalize_source_identity(
      source = normalized_df[["source"]][[index]],
      url = normalized_df[["url"]][[index]],
      name = normalized_df[["name"]][[index]],
      metadata = metadata_column[[index]]
    )
  }, character(1))

  normalized_df[["record_id"]] <- ifelse(!is.na(source_identity) & nzchar(source_identity), source_identity, normalized_df[["record_id"]])
  completeness <- .normalize_row_completeness(normalized_df)
  score_values <- suppressWarnings(as.numeric(normalized_df[["pre_llm_score"]]))
  score_values[is.na(score_values)] <- -Inf
  date_values <- suppressWarnings(as.numeric(vapply(normalized_df[["date_found"]], .normalize_parse_date, as.Date(NA))))
  date_values[is.na(date_values)] <- -Inf
  source_values <- tolower(as.character(normalized_df[["source"]]))
  source_values[is.na(source_values) | !nzchar(source_values)] <- "unknown"

  order_index <- order(-score_values, -completeness, -date_values, canonical_name, na.last = TRUE)
  normalized_df <- normalized_df[order_index, , drop = FALSE]
  canonical_url <- canonical_url[order_index]
  canonical_name <- canonical_name[order_index]
  source_identity <- source_identity[order_index]
  source_values <- source_values[order_index]

  dedup_key <- ifelse(
    !is.na(source_identity) & nzchar(source_identity),
    source_identity,
    ifelse(
      !is.na(canonical_url) & nzchar(canonical_url),
      paste(source_values, canonical_url, sep = "::"),
      paste(source_values, canonical_name, sep = "::")
    )
  )

  normalized_df[!duplicated(dedup_key), , drop = FALSE]
}

#' Normalize and merge raw source data
#'
#' @param raw_list Optional named list with raw source tables.
#' @param data_dir Directory containing intermediate artifacts.
#' @param deduplicate Whether to apply a primary deduplication step.
#' @param output_path Output path for the serialized normalized dataset.
#' @param duckdb_path DuckDB database path for primary/current normalized storage.
#' @param write_duckdb Whether to persist current and historical normalized snapshots into DuckDB.
#' @param run_id Optional pipeline run identifier for DuckDB history rows.
#'
#' @return Tibble with the canonical normalized schema.
normalize_raw_data <- function(
  raw_list = NULL,
  data_dir = get_default_data_dir(),
  deduplicate = TRUE,
  output_path = file.path(data_dir, "normalized_tools.rds"),
  duckdb_path = get_default_duckdb_path(data_dir),
  write_duckdb = TRUE,
  run_id = NA_character_
) {
  inputs <- raw_list

  if (is.null(inputs)) {
    inputs <- list(
      github = load_pipeline_table("raw_github", rds_path = file.path(data_dir, "raw_github.rds"), db_path = duckdb_path, required = FALSE),
      packetstorm = load_pipeline_table("raw_packetstorm", rds_path = file.path(data_dir, "raw_packetstorm.rds"), db_path = duckdb_path, required = FALSE),
      rss = load_pipeline_table("raw_rss", rds_path = file.path(data_dir, "raw_rss.rds"), db_path = duckdb_path, required = FALSE)
    )
  }

  normalized <- dplyr::bind_rows(
    .normalize_github(inputs$github),
    .normalize_packetstorm(inputs$packetstorm),
    .normalize_rss(inputs$rss)
  )

  normalized <- normalized[
    !is.na(normalized[["name"]]) & !is.na(normalized[["raw_text"]]),
    ,
    drop = FALSE
  ]

  if (isTRUE(deduplicate)) {
    normalized <- .normalize_deduplicate(normalized)
  }

  save_pipeline_rds(normalized, output_path)

  if (isTRUE(write_duckdb)) {
    write_pipeline_snapshot_table(normalized, "normalized_candidates", db_path = duckdb_path, history_table = "normalized_candidates_history", run_id = run_id, pipeline_stage = "normalize")
  }

  log_message(sprintf("Saved %s normalized rows to %s", nrow(normalized), output_path))
  normalized
}
