if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "band_qualifier",
    "context",
    "end_date",
    "family_rank",
    "family_seq",
    "full_name",
    "html_url",
    "language",
    "noun",
    "owner",
    "query",
    "query_family",
    "query_min_stars",
    "query_order",
    "query_tier",
    "repo",
    "search_page",
    "search_score",
    "sort_rank",
    "source_query",
    "source_query_family",
    "source_query_tier",
    "stargazers_count",
    "start_date",
    "term",
    "tier_rank",
    "topic"
  ))
}

#' Get default GitHub search queries
#'
#' Returns a curated set of search phrases for offensive security tools.
#'
#' @return Character vector of default GitHub search queries.
get_default_github_queries <- function() {
  unique(get_default_github_query_specs()$query)
}

#' Get curated GitHub organizations that frequently publish offensive utilities
#'
#' @return Character vector of GitHub organization names.
get_default_github_org_names <- function() {
  c(
    "projectdiscovery",
    "BishopFox",
    "Orange-Cyberdefense",
    "BC-SECURITY",
    "Ne0nd0g",
    "mgeeky",
    "carlospolop",
    "FalconForceTeam",
    "Teach2Breach",
    "Pennyw0rth",
    "JumpsecLabs",
    "FalconOpsLLC",
    "RedTeamOperations",
    "S1ckB0y1337",
    "CCob",
    "D3Ext"
  )
}

#' Get default GitHub discovery topics
#'
#' Returns a curated list of GitHub topics that can surface newly appearing
#' tools even when their names are not known in advance.
#'
#' @return Character vector of topic values.
get_default_github_topics <- function() {
  c(
    "red-team",
    "offensive-security",
    "pentest",
    "c2",
    "adversary-simulation",
    "post-exploitation",
    "lateral-movement",
    "credential-dumping",
    "active-directory",
    "reconnaissance",
    "osint",
    "phishing",
    "reverse-shell",
    "payload-encoding",
    "web-exploitation",
    "web-shell",
    "agent-framework",
    "implant",
    "tunneling",
    "pivoting",
    "cloud-security",
    "aws-pentesting",
    "azure-pentesting",
    "kubernetes-security",
    "container-security",
    "api-security",
    "windows-security",
    "linux-security",
    "edr-bypass",
    "av-evasion",
    "defense-evasion",
    "credential-access",
    "privilege-escalation",
    "persistence",
    "discovery",
    "collection",
    "exfiltration"
  )
}

#' Get default GitHub search sort modes
#'
#' Uses a blend of stable popularity search and freshness-oriented discovery.
#'
#' @return Character vector of GitHub repository search sort modes.
get_default_github_search_modes <- function() {
  c("updated", "best-match")
}

#' Get GitHub query families that should run before early stopping
#'
#' These families are aimed at finding recently created or recently pushed
#' repositories, including low-star projects that popularity-first search can
#' miss.
#'
#' @return Character vector of GitHub query family names.
get_default_github_fresh_discovery_families <- function() {
  c(
    "recent_discovery",
    "rolling_windows",
    "topic_discovery",
    "topic_language",
    "star_band_discovery",
    "language_shards",
    "name_fragment_discovery"
  )
}

#' Get default GitHub discovery languages
#'
#' @return Character vector of language shards used for query diversification.
get_default_github_discovery_languages <- function() {
  c(
    "Go",
    "Python",
    "Rust",
    "PowerShell",
    "C",
    "C++",
    "C#",
    "JavaScript"
  )
}

#' Get default GitHub discovery noun fragments
#'
#' @return Character vector of utility-oriented search nouns.
get_default_github_discovery_nouns <- function() {
  c(
    "loader",
    "implant",
    "beacon",
    "agent",
    "framework",
    "toolkit",
    "utility",
    "enumerator",
    "scanner",
    "sprayer",
    "tunnel",
    "pivot",
    "payload",
    "dropper",
    "stager"
  )
}

#' Get default GitHub discovery context keywords
#'
#' @return Character vector of context anchors for sharded discovery queries.
get_default_github_discovery_contexts <- function() {
  c(
    "red team",
    "offensive security",
    "post exploitation",
    "active directory",
    "cloud offensive",
    "credential access",
    "lateral movement",
    "adversary emulation"
  )
}

#' Get default GitHub rolling-window terms
#'
#' @return Character vector of freshness-oriented GitHub search phrases.
get_default_github_window_terms <- function() {
  c(
    "red team tool",
    "offensive security tool",
    "c2 framework",
    "post exploitation tool",
    "active directory tool",
    "credential access tool",
    "lateral movement tool",
    "cloud pentest tool",
    "payload framework",
    "recon tool"
  )
}

#' Get default GitHub star-band shards
#'
#' @return Tibble with star-band labels and qualifiers.
get_default_github_star_band_specs <- function() {
  tibble::tibble(
    band_label = c("zero_to_one", "two_to_five", "six_to_fifteen", "sixteen_to_fifty", "fifty_plus"),
    band_qualifier = c("stars:0..1", "stars:2..5", "stars:6..15", "stars:16..50", "stars:>=51")
  )
}

#' Get default GitHub rolling recency windows
#'
#' @param window_days Width of each rolling window.
#' @param lookback_windows Number of windows to generate.
#' @param end_date Right edge of the newest window.
#'
#' @return Tibble with start and end dates for rolling window discovery.
get_default_github_rolling_windows <- function(
  window_days = 30L,
  lookback_windows = 24L,
  end_date = Sys.Date()
) {
  window_days <- max(1L, as.integer(window_days))
  lookback_windows <- max(1L, as.integer(lookback_windows))
  end_date <- as.Date(end_date)

  indexes <- seq_len(lookback_windows) - 1L
  window_end_dates <- end_date - (indexes * window_days)
  window_start_dates <- window_end_dates - (window_days - 1L)

  tibble::tibble(
    window_index = indexes + 1L,
    end_date = format(window_end_dates, "%Y-%m-%d"),
    start_date = format(window_start_dates, "%Y-%m-%d")
  )
}

#' Get default GitHub seed tool names
#'
#' Returns a curated list of well-known offensive security tool names that can
#' be used as additional GitHub search seeds.
#'
#' @return Character vector of default seed tool names.
get_default_github_seed_names <- function() {
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
    "socat",
    "frp",
    "fscan",
    "ffuf",
    "dirsearch",
    "feroxbuster",
    "wfuzz",
    "commix",
    "mimikatz",
    "kerbrute",
    "sprayhound",
    "soaphound",
    "adexplorer",
    "adidnsdump",
    "enum4linux",
    "evilosx",
    "tokensmith",
    "tokenplayer",
    "lsassy",
    "gobuster",
    "naabu",
    "subfinder",
    "httpx",
    "nxc",
    "pillage",
    "ghostpack",
    "deimosc2",
    "viper",
    "tempest"
  )
}

#' Get structured GitHub query specs for utility-first discovery
#'
#' @return Tibble with query text, family, tier, minimum stars, and sort modes.
get_default_github_query_specs <- function() {
  precision_queries <- c(
    "offensive security framework",
    "red team automation tool",
    "adversary emulation framework",
    "command and control framework",
    "post exploitation framework",
    "active directory attack tool",
    "active directory enumeration tool",
    "credential access tool",
    "lateral movement framework",
    "payload delivery framework",
    "payload generation tool",
    "pivoting tool",
    "tunneling tool",
    "operator automation framework",
    "implant framework",
    "reconnaissance automation tool"
  )

  balanced_queries <- c(
    "cloud offensive security tool",
    "aws pentest tool",
    "azure offensive tool",
    "kubernetes offensive tool",
    "container escape tool",
    "web exploitation framework",
    "web shell framework",
    "api exploitation tool",
    "reverse shell framework",
    "persistence automation tool",
    "edr bypass tool",
    "antivirus evasion framework",
    "windows offensive utility",
    "linux offensive utility",
    "credential dumping utility",
    "phishing toolkit"
  )

  recent_queries <- c(
    sprintf("red team tool created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("offensive security tool created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("c2 framework created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("post exploitation tool created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("active directory tool created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("recon tool created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("cloud pentest tool created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d")),
    sprintf("payload framework created:>=%s", format(Sys.Date() - 180, "%Y-%m-%d"))
  )

  seed_queries <- sprintf('"%s" in:name,description', get_default_github_seed_names())
  topic_queries <- sprintf("topic:%s", get_default_github_topics())
  org_queries <- sprintf("org:%s", get_default_github_org_names())
  rolling_windows <- get_default_github_rolling_windows()
  rolling_queries <- merge(
    tibble::tibble(term = get_default_github_window_terms()),
    rolling_windows,
    by = NULL
  )
  rolling_queries$query <- sprintf(
    "%s pushed:%s..%s",
    rolling_queries[["term"]],
    rolling_queries[["start_date"]],
    rolling_queries[["end_date"]]
  )
  rolling_queries <- rolling_queries[, "query", drop = FALSE]
  language_queries <- merge(
    tibble::tibble(context = get_default_github_discovery_contexts()),
    merge(
      tibble::tibble(noun = get_default_github_discovery_nouns()),
      tibble::tibble(language = get_default_github_discovery_languages()),
      by = NULL
    ),
    by = NULL
  )
  language_queries$query <- sprintf(
    '"%s" %s language:%s',
    language_queries[["context"]],
    language_queries[["noun"]],
    language_queries[["language"]]
  )
  language_queries <- language_queries[, "query", drop = FALSE]
  topic_language_queries <- merge(
    tibble::tibble(topic = get_default_github_topics()),
    tibble::tibble(language = get_default_github_discovery_languages()),
    by = NULL
  )
  topic_language_queries$query <- sprintf(
    "topic:%s language:%s",
    topic_language_queries[["topic"]],
    topic_language_queries[["language"]]
  )
  topic_language_queries <- topic_language_queries[, "query", drop = FALSE]
  star_band_queries <- merge(
    tibble::tibble(term = get_default_github_window_terms()),
    get_default_github_star_band_specs(),
    by = NULL
  )
  star_band_queries$query <- sprintf(
    "%s %s pushed:>=%s",
    star_band_queries[["term"]],
    star_band_queries[["band_qualifier"]],
    format(Sys.Date() - 720, "%Y-%m-%d")
  )
  star_band_queries <- star_band_queries[, "query", drop = FALSE]
  name_fragment_queries <- sprintf(
    "%s in:name,description,readme",
    get_default_github_discovery_nouns()
  )

  specs <- dplyr::bind_rows(
    tibble::tibble(
      query = precision_queries,
      query_family = "precision_keywords",
      query_tier = "precision",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("updated", "best-match")), length(precision_queries)),
      query_pages = rep(list(1:2), length(precision_queries))
    ),
    tibble::tibble(
      query = balanced_queries,
      query_family = "balanced_keywords",
      query_tier = "balanced",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("updated", "best-match")), length(balanced_queries)),
      query_pages = rep(list(1:3), length(balanced_queries))
    ),
    tibble::tibble(
      query = recent_queries,
      query_family = "recent_discovery",
      query_tier = "broad",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("updated", "best-match")), length(recent_queries)),
      query_pages = rep(list(1:3), length(recent_queries))
    ),
    tibble::tibble(
      query = seed_queries,
      query_family = "seed_names",
      query_tier = "precision",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("updated", "best-match")), length(seed_queries)),
      query_pages = rep(list(1:2), length(seed_queries))
    ),
    tibble::tibble(
      query = topic_queries,
      query_family = "topic_discovery",
      query_tier = "balanced",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("updated", "best-match")), length(topic_queries)),
      query_pages = rep(list(1:3), length(topic_queries))
    ),
    tibble::tibble(
      query = org_queries,
      query_family = "org_publishers",
      query_tier = "precision",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("updated", "best-match")), length(org_queries)),
      query_pages = rep(list(1:2), length(org_queries))
    ),
    tibble::tibble(
      query = rolling_queries$query,
      query_family = "rolling_windows",
      query_tier = "broad",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("best-match", "updated")), nrow(rolling_queries)),
      query_pages = rep(list(1:2), nrow(rolling_queries))
    ),
    tibble::tibble(
      query = language_queries$query,
      query_family = "language_shards",
      query_tier = "balanced",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("best-match", "updated")), nrow(language_queries)),
      query_pages = rep(list(1:2), nrow(language_queries))
    ),
    tibble::tibble(
      query = topic_language_queries$query,
      query_family = "topic_language",
      query_tier = "balanced",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("best-match", "updated")), nrow(topic_language_queries)),
      query_pages = rep(list(1:2), nrow(topic_language_queries))
    ),
    tibble::tibble(
      query = star_band_queries$query,
      query_family = "star_band_discovery",
      query_tier = "broad",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("best-match", "updated")), nrow(star_band_queries)),
      query_pages = rep(list(1:3), nrow(star_band_queries))
    ),
    tibble::tibble(
      query = name_fragment_queries,
      query_family = "name_fragment_discovery",
      query_tier = "balanced",
      query_min_stars = 0L,
      query_sort_modes = rep(list(c("best-match", "updated")), length(name_fragment_queries)),
      query_pages = rep(list(1:3), length(name_fragment_queries))
    )
  )

  specs <- specs[
    !duplicated(specs[, c("query", "query_family", "query_tier", "query_min_stars")]),
    ,
    drop = FALSE
  ]

  tibble::as_tibble(specs)
}

.github_scalar <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || (is.character(first_value) && !nzchar(first_value))) {
    return(default)
  }

  as.character(first_value)
}

.github_character_vector <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(character(0))
  }

  unique(as.character(unlist(value, use.names = FALSE)))
}

.github_integer <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_integer_)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || is.na(first_value)) {
    return(NA_integer_)
  }

  as.integer(first_value)
}

.github_numeric <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || is.na(first_value)) {
    return(NA_real_)
  }

  as.numeric(first_value)
}

.github_token <- function(token = NULL) {
  if (!is.null(token)) {
    return(as.character(token))
  }

  get_runtime_env_value("GITHUB_PAT", unset = "")
}

.github_request <- function(
  endpoint,
  ...,
  token = NULL,
  error_context = "GitHub API request failed"
) {
  token_value <- .github_token(token)
  request_args <- list(
    ...,
    .send_headers = c(
      Accept = "application/vnd.github+json",
      `X-GitHub-Api-Version` = "2022-11-28"
    )
  )

  if (nzchar(token_value)) {
    request_args$.token <- token_value
  }

  safe_run(
    expr = function() {
      do.call(gh::gh, c(list(endpoint = endpoint), request_args))
    },
    error_context = error_context
  )
}

.github_decode_readme <- function(readme_response) {
  if (is.null(readme_response)) {
    return(NA_character_)
  }

  content <- .github_scalar(readme_response$content, default = "")
  encoding <- .github_scalar(readme_response$encoding, default = "")

  if (!nzchar(content)) {
    return(NA_character_)
  }

  if (!identical(encoding, "base64")) {
    return(content)
  }

  safe_run(
    expr = function() {
      cleaned_content <- stringr::str_replace_all(content, "\\s+", "")
      decoded <- jsonlite::base64_dec(cleaned_content)
      enc2utf8(rawToChar(decoded))
    },
    error_context = "Failed to decode GitHub README content"
  )
}

.github_build_search_query <- function(
  query,
  min_stars = 0L,
  languages = NULL,
  include_archived = FALSE
) {
  qualifiers <- character(0)
  query_text <- dplyr::coalesce(.github_scalar(query, default = ""), "")
  has_stars_qualifier <- stringr::str_detect(query_text, stringr::regex("(^|\\s)stars:", ignore_case = TRUE))
  has_language_qualifier <- stringr::str_detect(query_text, stringr::regex("(^|\\s)language:", ignore_case = TRUE))

  if (!has_stars_qualifier && !is.null(min_stars) && !is.na(min_stars) && min_stars > 0) {
    qualifiers <- c(qualifiers, sprintf("stars:>=%s", as.integer(min_stars)))
  }

  if (!isTRUE(include_archived)) {
    qualifiers <- c(qualifiers, "archived:false")
  }

  if (!has_language_qualifier && !is.null(languages) && length(languages) > 0) {
    language_qualifiers <- sprintf("language:%s", languages)
    qualifiers <- c(qualifiers, language_qualifiers)
  }

  stringr::str_squish(paste(c(query_text, qualifiers), collapse = " "))
}

.github_normalize_query_specs <- function(queries, default_min_stars = 0L, default_sort_modes = get_default_github_search_modes()) {
  cleaned_sort_modes <- unique(stats::na.omit(as.character(default_sort_modes)))
  cleaned_sort_modes <- cleaned_sort_modes[nzchar(cleaned_sort_modes)]
  default_pages <- 1L

  if (is.data.frame(queries)) {
    specs <- tibble::as_tibble(queries)

    if (!("query" %in% names(specs))) {
      stop("GitHub query specs must contain a 'query' column.", call. = FALSE)
    }

    if (!("query_family" %in% names(specs))) {
      specs$query_family <- "custom"
    }

    if (!("query_tier" %in% names(specs))) {
      specs$query_tier <- "custom"
    }

    if (!("query_min_stars" %in% names(specs))) {
      specs$query_min_stars <- as.integer(default_min_stars)
    }

    if (!("query_sort_modes" %in% names(specs))) {
      specs$query_sort_modes <- rep(list(cleaned_sort_modes), nrow(specs))
    } else {
      sort_values <- specs$query_sort_modes
      if (!is.list(sort_values)) {
        sort_values <- as.list(sort_values)
      }

      specs$query_sort_modes <- lapply(
        sort_values,
        function(value) {
          normalized <- unique(stats::na.omit(as.character(unlist(value, use.names = FALSE))))
          normalized <- normalized[nzchar(normalized)]
          if (length(normalized) == 0) cleaned_sort_modes else normalized
        }
      )
    }

    if (!("query_pages" %in% names(specs))) {
      specs$query_pages <- rep(list(default_pages), nrow(specs))
    } else {
      page_values <- specs$query_pages
      if (!is.list(page_values)) {
        page_values <- as.list(page_values)
      }

      specs$query_pages <- lapply(
        page_values,
        function(value) {
          normalized <- suppressWarnings(as.integer(unlist(value, use.names = FALSE)))
          normalized <- unique(stats::na.omit(normalized))
          normalized <- normalized[normalized >= 1L]
          if (length(normalized) == 0) default_pages else normalized
        }
      )
    }

    specs$query <- as.character(specs$query)
    specs$query_family <- as.character(specs$query_family)
    specs$query_tier <- as.character(specs$query_tier)
    specs$query_min_stars <- suppressWarnings(as.integer(specs$query_min_stars))
  } else {
    cleaned_queries <- unique(stats::na.omit(as.character(queries)))
    cleaned_queries <- cleaned_queries[nzchar(cleaned_queries)]

    specs <- tibble::tibble(
      query = cleaned_queries,
      query_family = rep("custom", length(cleaned_queries)),
      query_tier = rep("custom", length(cleaned_queries)),
      query_min_stars = rep(as.integer(default_min_stars), length(cleaned_queries)),
      query_sort_modes = rep(list(cleaned_sort_modes), length(cleaned_queries)),
      query_pages = rep(list(default_pages), length(cleaned_queries))
    )
  }

  specs <- specs[!is.na(specs[["query"]]) & nzchar(specs[["query"]]), , drop = FALSE]
  specs[["query_family"]][is.na(specs[["query_family"]])] <- "custom"
  specs[["query_tier"]][is.na(specs[["query_tier"]])] <- "custom"
  missing_min_stars <- is.na(specs[["query_min_stars"]])
  specs[["query_min_stars"]][missing_min_stars] <- as.integer(default_min_stars)

  tibble::as_tibble(specs)
}

.github_build_query_plan <- function(query_specs) {
  if (!is.data.frame(query_specs) || nrow(query_specs) == 0) {
    return(tibble::tibble(
      query = character(),
      query_family = character(),
      query_tier = character(),
      query_min_stars = integer(),
      search_sort = character(),
      search_page = integer(),
      query_order = integer()
    ))
  }

  plan <- dplyr::bind_rows(lapply(
    seq_len(nrow(query_specs)),
    function(index) {
      spec <- query_specs[index, , drop = FALSE]
      pages <- suppressWarnings(as.integer(unlist(spec$query_pages[[1]], use.names = FALSE)))
      pages <- unique(stats::na.omit(pages))
      pages <- pages[pages >= 1L]

      if (length(pages) == 0) {
        pages <- 1L
      }

      dplyr::bind_rows(lapply(
        pages,
        function(page) {
          tibble::tibble(
            query = spec$query[[1]],
            query_family = spec$query_family[[1]],
            query_tier = spec$query_tier[[1]],
            query_min_stars = spec$query_min_stars[[1]],
            search_sort = spec$query_sort_modes[[1]],
            search_page = as.integer(page),
            query_order = index
          )
        }
      ))
    }
  ))

  plan[["tier_rank"]] <- ifelse(
    plan[["query_tier"]] == "precision",
    1L,
    ifelse(plan[["query_tier"]] == "balanced", 2L, ifelse(plan[["query_tier"]] == "broad", 3L, 4L))
  )
  plan[["family_rank"]] <- match(plan[["query_family"]], unique(plan[["query_family"]]))
  plan[["sort_rank"]] <- ifelse(
    plan[["search_sort"]] == "updated",
    1L,
    ifelse(plan[["search_sort"]] == "best-match", 2L, ifelse(plan[["search_sort"]] == "stars", 3L, 4L))
  )
  plan[["family_seq"]] <- ave(seq_len(nrow(plan)), plan[["query_family"]], FUN = seq_along)

  order_index <- order(
    plan[["search_page"]],
    plan[["sort_rank"]],
    plan[["family_seq"]],
    plan[["tier_rank"]],
    plan[["family_rank"]],
    plan[["query_order"]],
    na.last = TRUE
  )

  plan <- plan[order_index, , drop = FALSE]
  plan[, c("query", "query_family", "query_tier", "query_min_stars", "search_sort", "search_page", "query_order"), drop = FALSE]
}

.github_empty_search_results <- function() {
  tibble::tibble(
    source_query = character(),
    source_query_family = character(),
    source_query_tier = character(),
    query_min_stars = integer(),
    search_query = character(),
    search_sort = character(),
    search_page = integer(),
    owner = character(),
    repo = character(),
    full_name = character(),
    html_url = character(),
    description = character(),
    language = character(),
    archived = logical(),
    stargazers_count = integer(),
    created_at = character(),
    updated_at = character(),
    pushed_at = character(),
    search_score = numeric()
  )
}

.github_empty_detail_results <- function() {
  tibble::tibble(
    source = character(),
    source_type = character(),
    owner = character(),
    repo = character(),
    full_name = character(),
    html_url = character(),
    api_url = character(),
    description = character(),
    language = character(),
    topics = list(),
    stargazers_count = integer(),
    forks_count = integer(),
    open_issues_count = integer(),
    archived = logical(),
    default_branch = character(),
    license_key = character(),
    license_name = character(),
    homepage = character(),
    created_at = character(),
    updated_at = character(),
    pushed_at = character(),
    readme = character(),
    matched_queries = list(),
    matched_query_families = list(),
    matched_query_tiers = list(),
    matched_query_text = character(),
    matched_query_family_text = character(),
    matched_query_tier_text = character(),
    matched_search_modes = list(),
    matched_search_mode_text = character(),
    matched_search_pages = list(),
    matched_search_page_text = character(),
    search_score = numeric()
  )
}

.github_empty_search_log <- function() {
  tibble::tibble(
    executed_at = character(),
    request_index = integer(),
    source_query = character(),
    query_family = character(),
    query_tier = character(),
    query_min_stars = integer(),
    search_query = character(),
    search_sort = character(),
    search_page = integer(),
    request_status = character(),
    returned_rows = integer(),
    unique_candidates_after_request = integer()
  )
}

.github_save_search_log <- function(search_log, log_path) {
  if (is.null(log_path) || !nzchar(log_path)) {
    return(invisible(search_log))
  }

  save_pipeline_rds(search_log, log_path)
  json_path <- sub("[.]rds$", ".json", log_path)

  if (!identical(json_path, log_path)) {
    jsonlite::write_json(search_log, json_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  }

  invisible(search_log)
}

.github_extract_search_items <- function(response, query, query_family, query_tier, query_min_stars, search_query, search_sort, search_page) {
  items <- response$items

  if (is.null(items) || length(items) == 0) {
    return(.github_empty_search_results())
  }

  rows <- lapply(
    items,
    function(item) {
      tibble::tibble(
        source_query = query,
        source_query_family = query_family,
        source_query_tier = query_tier,
        query_min_stars = as.integer(query_min_stars),
        search_query = search_query,
        search_sort = search_sort,
        search_page = as.integer(search_page),
        owner = .github_scalar(item$owner$login),
        repo = .github_scalar(item$name),
        full_name = .github_scalar(item$full_name),
        html_url = .github_scalar(item$html_url),
        description = .github_scalar(item$description),
        language = .github_scalar(item$language),
        archived = isTRUE(item$archived),
        stargazers_count = .github_integer(item$stargazers_count),
        created_at = .github_scalar(item$created_at),
        updated_at = .github_scalar(item$updated_at),
        pushed_at = .github_scalar(item$pushed_at),
        search_score = .github_numeric(item$score)
      )
    }
  )

  dplyr::bind_rows(rows)
}

#' Fetch normalized metadata for a GitHub repository
#'
#' @param owner Repository owner or organization.
#' @param repo Repository name.
#' @param include_readme Whether to fetch and decode the README.
#' @param token Optional GitHub personal access token.
#'
#' @return Tibble with one normalized repository row.
fetch_repo_details <- function(
  owner,
  repo,
  include_readme = TRUE,
  token = NULL
) {
  if (!nzchar(owner) || !nzchar(repo)) {
    stop("Both 'owner' and 'repo' must be provided.", call. = FALSE)
  }

  repository <- .github_request(
    endpoint = "/repos/{owner}/{repo}",
    owner = owner,
    repo = repo,
    token = token,
    error_context = sprintf("Failed to fetch repository details for %s/%s", owner, repo)
  )

  readme_response <- NULL

  if (isTRUE(include_readme)) {
    readme_response <- tryCatch(
      .github_request(
        endpoint = "/repos/{owner}/{repo}/readme",
        owner = owner,
        repo = repo,
        token = token,
        error_context = sprintf("Failed to fetch README for %s/%s", owner, repo)
      ),
      error = function(error) {
        log_message(
          sprintf("README unavailable for %s/%s: %s", owner, repo, conditionMessage(error)),
          level = "WARN"
        )
        NULL
      }
    )
  }

  tibble::tibble(
    source = "github",
    source_type = "repository",
    owner = .github_scalar(repository$owner$login, default = owner),
    repo = .github_scalar(repository$name, default = repo),
    full_name = .github_scalar(repository$full_name, default = sprintf("%s/%s", owner, repo)),
    html_url = .github_scalar(repository$html_url),
    api_url = .github_scalar(repository$url),
    description = .github_scalar(repository$description),
    language = .github_scalar(repository$language),
    topics = list(.github_character_vector(repository$topics)),
    stargazers_count = .github_integer(repository$stargazers_count),
    forks_count = .github_integer(repository$forks_count),
    open_issues_count = .github_integer(repository$open_issues_count),
    archived = isTRUE(repository$archived),
    default_branch = .github_scalar(repository$default_branch),
    license_key = .github_scalar(repository$license$key),
    license_name = .github_scalar(repository$license$name),
    homepage = .github_scalar(repository$homepage),
    created_at = .github_scalar(repository$created_at),
    updated_at = .github_scalar(repository$updated_at),
    pushed_at = .github_scalar(repository$pushed_at),
    readme = .github_decode_readme(readme_response),
    matched_queries = list(character(0)),
    matched_query_families = list(character(0)),
    matched_query_tiers = list(character(0)),
    matched_query_text = NA_character_,
    matched_query_family_text = NA_character_,
    matched_query_tier_text = NA_character_,
    matched_search_pages = list(character(0)),
    matched_search_page_text = NA_character_,
    search_score = NA_real_
  )
}

#' Search GitHub for offensive security repositories
#'
#' @param queries Character vector of search queries.
#' @param min_stars Minimum number of stars required.
#' @param max_results Maximum number of unique repositories to return.
#' @param languages Optional character vector of language qualifiers.
#' @param include_archived Whether archived repositories should be included.
#' @param include_readme Whether to fetch and decode READMEs.
#' @param token Optional GitHub personal access token.
#'
#' @return Tibble with normalized GitHub repository metadata.
search_github_tools <- function(
  queries,
  min_stars = 0L,
  max_results = 50L,
  languages = NULL,
  include_archived = FALSE,
  include_readme = TRUE,
  sort_modes = get_default_github_search_modes(),
  max_search_requests = 60L,
  min_search_requests = 12L,
  token = NULL,
  search_log_path = NULL
) {
  query_specs <- .github_normalize_query_specs(
    queries = queries,
    default_min_stars = min_stars,
    default_sort_modes = sort_modes
  )
  cleaned_sort_modes <- unique(stats::na.omit(as.character(sort_modes)))
  cleaned_sort_modes <- cleaned_sort_modes[nzchar(cleaned_sort_modes)]

  if (nrow(query_specs) == 0) {
    stop("At least one non-empty GitHub search query is required.", call. = FALSE)
  }

  if (length(cleaned_sort_modes) == 0) {
    stop("At least one GitHub search sort mode is required.", call. = FALSE)
  }

  unsupported_sort_modes <- setdiff(cleaned_sort_modes, c("best-match", "stars", "updated"))

  if (length(unsupported_sort_modes) > 0) {
    stop(
      sprintf("Unsupported GitHub search sort modes: %s", paste(unsupported_sort_modes, collapse = ", ")),
      call. = FALSE
    )
  }

  max_results <- as.integer(max_results)
  max_search_requests <- as.integer(max_search_requests)
  min_search_requests <- as.integer(min_search_requests)

  if (is.na(max_results) || max_results < 1L) {
    stop("'max_results' must be a positive integer.", call. = FALSE)
  }

  if (is.na(max_search_requests) || max_search_requests < 1L) {
    stop("'max_search_requests' must be a positive integer.", call. = FALSE)
  }

  if (is.na(min_search_requests) || min_search_requests < 1L) {
    stop("'min_search_requests' must be a positive integer.", call. = FALSE)
  }

  min_search_requests <- min(min_search_requests, max_search_requests)

  query_plan <- .github_build_query_plan(query_specs)

  per_query <- min(100L, max(10L, ceiling(max_results / max(1L, nrow(query_plan))) * 4L))

  log_message(sprintf(
    "GitHub search plan: specs=%s request_slots=%s max_results=%s per_query=%s",
    nrow(query_specs),
    min(nrow(query_plan), max_search_requests),
    max_results,
    per_query
  ))

  search_rows <- list()
  search_log_rows <- list()
  request_count <- 0L
  enough_candidates <- FALSE
  discovery_families <- intersect(
    get_default_github_fresh_discovery_families(),
    unique(query_plan[["query_family"]])
  )
  executed_discovery_families <- character(0)

  for (index in seq_len(nrow(query_plan))) {
    if (request_count >= max_search_requests || isTRUE(enough_candidates)) {
      break
    }

    query <- query_plan$query[[index]]
    query_family <- query_plan$query_family[[index]]
    query_tier <- query_plan$query_tier[[index]]
    query_min_stars <- query_plan$query_min_stars[[index]]
    search_sort <- query_plan$search_sort[[index]]
    search_page <- query_plan$search_page[[index]]
    search_query <- .github_build_search_query(
      query = query,
      min_stars = max(as.integer(min_stars), as.integer(query_min_stars)),
      languages = languages,
      include_archived = include_archived
    )

    log_message(sprintf(
      "GitHub request %s/%s: family=%s tier=%s sort=%s page=%s query=%s",
      request_count + 1L,
      min(nrow(query_plan), max_search_requests),
      query_family,
      query_tier,
      search_sort,
      search_page,
      search_query
    ))

    request_status <- "success"

    result <- tryCatch(
      {
        request_args <- list(
          endpoint = "/search/repositories",
          q = search_query,
          per_page = per_query,
          page = as.integer(search_page),
          token = token,
          error_context = sprintf(
            "Failed to search GitHub repositories for query '%s' with sort '%s' page '%s'",
            query,
            search_sort,
            search_page
          )
        )

        if (!identical(search_sort, "best-match")) {
          request_args$sort <- search_sort
          request_args$order <- "desc"
        }

        response <- do.call(.github_request, request_args)

        .github_extract_search_items(
          response,
          query = query,
          query_family = query_family,
          query_tier = query_tier,
          query_min_stars = query_min_stars,
          search_query = search_query,
          search_sort = search_sort,
          search_page = search_page
        )
      },
      error = function(error) {
        request_status <<- "error"
        log_message(
          sprintf(
            "GitHub search query '%s' with sort '%s' page '%s' failed: %s",
            query,
            search_sort,
            search_page,
            conditionMessage(error)
          ),
          level = "WARN"
        )
        .github_empty_search_results()
      }
    )

    request_count <- request_count + 1L
    search_rows[[length(search_rows) + 1L]] <- result
    if (query_family %in% discovery_families) {
      executed_discovery_families <- unique(c(executed_discovery_families, query_family))
    }

    accumulated_results <- dplyr::bind_rows(search_rows)
    valid_full_names <- accumulated_results[["full_name"]]
    valid_full_names <- valid_full_names[!is.na(valid_full_names) & nzchar(valid_full_names)]
    unique_candidates <- length(unique(valid_full_names))

    log_message(sprintf(
      "GitHub request %s/%s finished: status=%s returned_rows=%s unique_candidates=%s",
      request_count,
      min(nrow(query_plan), max_search_requests),
      request_status,
      nrow(result),
      unique_candidates
    ))

    search_log_rows[[length(search_log_rows) + 1L]] <- tibble::tibble(
      executed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      request_index = as.integer(request_count),
      source_query = query,
      query_family = query_family,
      query_tier = query_tier,
      query_min_stars = as.integer(query_min_stars),
      search_query = search_query,
      search_sort = search_sort,
      search_page = as.integer(search_page),
      request_status = request_status,
      returned_rows = as.integer(nrow(result)),
      unique_candidates_after_request = as.integer(unique_candidates)
    )

    discovery_floor_reached <- length(setdiff(discovery_families, executed_discovery_families)) == 0
    if (request_count >= min_search_requests && discovery_floor_reached && unique_candidates >= (max_results * 3L)) {
      enough_candidates <- TRUE
    }
  }

  search_log <- dplyr::bind_rows(search_log_rows)
  if (!is.data.frame(search_log) || nrow(search_log) == 0) {
    search_log <- .github_empty_search_log()
  }
  .github_save_search_log(search_log, search_log_path)

  search_results <- dplyr::bind_rows(search_rows)

  if (nrow(search_results) == 0) {
    return(.github_empty_detail_results())
  }

  group_keys <- paste(search_results[["owner"]], search_results[["repo"]], search_results[["full_name"]], sep = "::")
  grouped_rows <- split(search_results, group_keys)

  deduplicated <- dplyr::bind_rows(lapply(
    grouped_rows,
    function(group) {
      score <- suppressWarnings(max(group[["search_score"]], na.rm = TRUE))
      if (!is.finite(score)) {
        score <- NA_real_
      }

      tibble::tibble(
        owner = .github_scalar(group[["owner"]]),
        repo = .github_scalar(group[["repo"]]),
        full_name = .github_scalar(group[["full_name"]]),
        html_url = .github_scalar(stats::na.omit(group[["html_url"]])),
        matched_queries = list(sort(unique(group[["source_query"]]))),
        matched_query_families = list(sort(unique(group[["source_query_family"]]))),
        matched_query_tiers = list(sort(unique(group[["source_query_tier"]]))),
        matched_query_text = paste(sort(unique(group[["source_query"]])), collapse = "; "),
        matched_query_family_text = paste(sort(unique(group[["source_query_family"]])), collapse = "; "),
        matched_query_tier_text = paste(sort(unique(group[["source_query_tier"]])), collapse = "; "),
        matched_search_modes = list(sort(unique(group[["search_sort"]]))),
        matched_search_mode_text = paste(sort(unique(group[["search_sort"]])), collapse = "; "),
        matched_search_pages = list(sort(unique(group[["search_page"]]))),
        matched_search_page_text = paste(sort(unique(group[["search_page"]])), collapse = "; "),
        created_at = .github_scalar(stats::na.omit(group[["created_at"]])),
        updated_at = .github_scalar(stats::na.omit(group[["updated_at"]])),
        pushed_at = .github_scalar(stats::na.omit(group[["pushed_at"]])),
        search_score = score
      )
    }
  ))

  deduplicated$search_score[!is.finite(deduplicated$search_score)] <- NA_real_

  dedup_scores <- deduplicated[["search_score"]]
  dedup_scores[is.na(dedup_scores)] <- -Inf
  created_values <- suppressWarnings(as.POSIXct(deduplicated[["created_at"]], tz = "UTC"))
  pushed_values <- suppressWarnings(as.POSIXct(deduplicated[["pushed_at"]], tz = "UTC"))
  created_scores <- as.numeric(created_values)
  pushed_scores <- as.numeric(pushed_values)
  created_scores[is.na(created_scores)] <- -Inf
  pushed_scores[is.na(pushed_scores)] <- -Inf
  candidate_order <- order(-created_scores, -pushed_scores, -dedup_scores, deduplicated[["full_name"]], na.last = TRUE)
  candidate_rows <- deduplicated[candidate_order, , drop = FALSE]
  candidate_rows <- utils::head(candidate_rows, max_results)

  detail_rows <- lapply(
    seq_len(nrow(candidate_rows)),
    function(index) {
      row <- candidate_rows[index, , drop = FALSE]

      tryCatch(
        {
          details <- fetch_repo_details(
            owner = row$owner[[1]],
            repo = row$repo[[1]],
            include_readme = include_readme,
            token = token
          )

          details$matched_queries <- list(row$matched_queries[[1]])
          details$matched_query_families <- list(row$matched_query_families[[1]])
          details$matched_query_tiers <- list(row$matched_query_tiers[[1]])
          details$matched_query_text <- row$matched_query_text[[1]]
          details$matched_query_family_text <- row$matched_query_family_text[[1]]
          details$matched_query_tier_text <- row$matched_query_tier_text[[1]]
          details$matched_search_modes <- list(row$matched_search_modes[[1]])
          details$matched_search_mode_text <- row$matched_search_mode_text[[1]]
          details$matched_search_pages <- list(row$matched_search_pages[[1]])
          details$matched_search_page_text <- row$matched_search_page_text[[1]]
          details$search_score <- row$search_score[[1]]
          details
        },
        error = function(error) {
          log_message(
            sprintf(
              "Failed to normalize GitHub repository %s: %s",
              row$full_name[[1]],
              conditionMessage(error)
            ),
            level = "WARN"
          )
          NULL
        }
      )
    }
  )

  normalized <- dplyr::bind_rows(detail_rows)

  if (nrow(normalized) == 0) {
    return(.github_empty_detail_results())
  }

  score_values <- normalized[["search_score"]]
  score_values[is.na(score_values)] <- -Inf
  created_values <- suppressWarnings(as.POSIXct(normalized[["created_at"]], tz = "UTC"))
  pushed_values <- suppressWarnings(as.POSIXct(normalized[["pushed_at"]], tz = "UTC"))
  created_scores <- as.numeric(created_values)
  pushed_scores <- as.numeric(pushed_values)
  created_scores[is.na(created_scores)] <- -Inf
  pushed_scores[is.na(pushed_scores)] <- -Inf
  normalized[order(-created_scores, -pushed_scores, -score_values, normalized[["full_name"]], na.last = TRUE), , drop = FALSE]
}

#' Collect and save raw GitHub repository data
#'
#' @param queries Character vector of search queries.
#' @param min_stars Minimum number of stars required.
#' @param max_results Maximum number of unique repositories to return.
#' @param languages Optional character vector of language qualifiers.
#' @param include_archived Whether archived repositories should be included.
#' @param include_readme Whether to fetch and decode READMEs.
#' @param token Optional GitHub personal access token.
#' @param output_path Output path for the serialized raw dataset.
#'
#' @return Tibble with normalized GitHub repository metadata.
collect_github_tools <- function(
  queries = get_default_github_query_specs(),
  min_stars = 0L,
  max_results = 50L,
  languages = NULL,
  include_archived = FALSE,
  include_readme = TRUE,
  sort_modes = get_default_github_search_modes(),
  max_search_requests = 60L,
  min_search_requests = 12L,
  token = NULL,
  search_log_path = file.path(get_default_data_dir(), "github_search_log.rds"),
  output_path = file.path(get_default_data_dir(), "raw_github.rds")
) {
  log_message("Collecting raw GitHub repository data")

  github_tools <- search_github_tools(
    queries = queries,
    min_stars = min_stars,
    max_results = max_results,
    languages = languages,
    include_archived = include_archived,
    include_readme = include_readme,
    sort_modes = sort_modes,
    max_search_requests = max_search_requests,
    min_search_requests = min_search_requests,
    token = token,
    search_log_path = search_log_path
  )

  save_pipeline_rds(github_tools, output_path)

  log_message(sprintf("Saved %s GitHub repository rows to %s", nrow(github_tools), output_path))

  github_tools
}
