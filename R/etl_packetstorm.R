#' Get default Packet Storm News sections
#'
#' @return Character vector of Packet Storm News sections.
get_default_packetstorm_categories <- function() {
  character(0)
}

get_default_packetstorm_api_url <- function() {
  get_runtime_env_value(
    "PACKETSTORM_API_URL",
    unset = "https://api.packetstormsecurity.com/v31337.20240702/api"
  )
}

get_default_packetstorm_manual_urls <- function() {
  raw_value <- get_runtime_env_value("PACKETSTORM_URLS", unset = "")

  if (!nzchar(raw_value)) {
    return(character(0))
  }

  values <- unlist(strsplit(raw_value, "[\r\n;]+", perl = TRUE), use.names = FALSE)
  values <- trimws(values)
  unique(values[nzchar(values)])
}

get_default_packetstorm_manual_urls_file <- function() {
  get_runtime_env_value("PACKETSTORM_URLS_FILE", unset = "")
}

.packetstorm_api_secret <- function(api_secret = NULL) {
  if (!is.null(api_secret)) {
    return(as.character(api_secret))
  }

  get_runtime_env_value("PACKETSTORM_API_SECRET", unset = "")
}

.packetstorm_scalar <- function(value, default = NA_character_) {
  if (is.null(value) || length(value) == 0) {
    return(default)
  }

  first_value <- value[[1]]

  if (is.null(first_value) || length(first_value) == 0 || is.na(first_value)) {
    return(default)
  }

  first_value <- as.character(first_value)

  if (!nzchar(first_value)) {
    return(default)
  }

  first_value
}

.packetstorm_integer <- function(value, default = NA_integer_) {
  scalar <- .packetstorm_scalar(value, default = NA_character_)

  if (is.na(scalar) || !nzchar(scalar)) {
    return(default)
  }

  as.integer(scalar)
}

.packetstorm_empty_manual_entries <- function() {
  tibble::tibble(
    category = character(),
    title = character(),
    url = character(),
    description = character(),
    pub_date = character(),
    tag_names = list()
  )
}

.packetstorm_empty_results <- function() {
  tibble::tibble(
    source = character(),
    source_type = character(),
    category = character(),
    title = character(),
    url = character(),
    guid = character(),
    description = character(),
    pub_date = character(),
    feed_url = character(),
    credit = character(),
    credit_url = character(),
    tag_names = list(),
    api_query = character(),
    item_id = integer(),
    page_content = character(),
    raw_content = character()
  )
}

.packetstorm_fetch_linked_content <- function(url, max_chars = 2500L) {
  tryCatch(
    .extract_linked_page_text(url, max_chars = max_chars),
    error = function(error) {
      log_message(
        sprintf("Linked PacketStorm content unavailable for %s: %s", url, conditionMessage(error)),
        level = "WARN"
      )
      NA_character_
    }
  )
}

.packetstorm_fetch_article_snapshot <- function(url, max_chars = 2500L) {
  tryCatch(
    safe_run(
      expr = function() {
        document <- rvest::read_html(url)
        removable_nodes <- rvest::html_elements(document, "script, style, noscript, svg")

        if (length(removable_nodes) > 0) {
          xml2::xml_remove(removable_nodes)
        }

        title_node <- rvest::html_element(document, "title")
        body_node <- rvest::html_element(document, "body")

        list(
          title = if (length(title_node) == 0 || inherits(title_node, "xml_missing")) {
            NA_character_
          } else {
            .clean_candidate_text(xml2::xml_text(title_node), max_chars = 240L)
          },
          page_content = if (length(body_node) == 0 || inherits(body_node, "xml_missing")) {
            NA_character_
          } else {
            .clean_candidate_text(xml2::xml_text(body_node), max_chars = max_chars)
          }
        )
      },
      error_context = sprintf("Failed to fetch Packet Storm article snapshot from %s", url)
    ),
    error = function(error) {
      log_message(
        sprintf("Packet Storm article snapshot unavailable for %s: %s", url, conditionMessage(error)),
        level = "WARN"
      )
      list(title = NA_character_, page_content = NA_character_)
    }
  )
}

.packetstorm_parse_manual_entry <- function(entry) {
  parts <- trimws(strsplit(as.character(entry), "\t", fixed = TRUE)[[1]])
  parts <- parts[!is.na(parts)]

  if (length(parts) == 0) {
    return(.packetstorm_empty_manual_entries())
  }

  if (length(parts) == 1) {
    return(tibble::tibble(
      category = NA_character_,
      title = NA_character_,
      url = parts[[1]],
      description = NA_character_,
      pub_date = NA_character_,
      tag_names = list(character(0))
    ))
  }

  if (length(parts) == 2) {
    return(tibble::tibble(
      category = parts[[1]],
      title = NA_character_,
      url = parts[[2]],
      description = NA_character_,
      pub_date = NA_character_,
      tag_names = list(character(0))
    ))
  }

  tibble::tibble(
    category = parts[[1]],
    title = parts[[2]],
    url = parts[[3]],
    description = if (length(parts) >= 4) parts[[4]] else NA_character_,
    pub_date = if (length(parts) >= 5) parts[[5]] else NA_character_,
    tag_names = list(
      if (length(parts) >= 6 && nzchar(parts[[6]])) {
        trimws(unlist(strsplit(parts[[6]], ",", fixed = TRUE), use.names = FALSE))
      } else {
        character(0)
      }
    )
  )
}

.packetstorm_read_manual_entries <- function(manual_urls = NULL, manual_urls_file = NULL) {
  url_values <- unique(stats::na.omit(as.character(manual_urls)))
  url_values <- trimws(url_values)
  url_values <- url_values[nzchar(url_values)]

  file_path <- .packetstorm_scalar(manual_urls_file, default = "")
  file_values <- character(0)

  if (nzchar(file_path) && file.exists(file_path)) {
    file_values <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  }

  entries <- c(url_values, file_values)
  entries <- trimws(entries)
  entries <- entries[nzchar(entries)]
  entries <- entries[!startsWith(entries, "#")]

  if (length(entries) == 0) {
    return(.packetstorm_empty_manual_entries())
  }

  parsed <- dplyr::bind_rows(lapply(entries, .packetstorm_parse_manual_entry))

  parsed |>
    dplyr::filter(!is.na(url), nzchar(url)) |>
    dplyr::distinct(url, .keep_all = TRUE)
}

.packetstorm_encode_body <- function(payload) {
  payload <- payload[!vapply(payload, is.null, logical(1))]

  paste(
    sprintf(
      "%s=%s",
      utils::URLencode(names(payload), reserved = TRUE),
      utils::URLencode(vapply(payload, as.character, character(1)), reserved = TRUE)
    ),
    collapse = "&"
  )
}

.packetstorm_request <- function(payload, api_url, api_secret) {
  request <- httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Apisecret = api_secret,
      `Content-Type` = "application/x-www-form-urlencoded"
    ) |>
    httr2::req_body_raw(
      .packetstorm_encode_body(payload),
      type = "application/x-www-form-urlencoded"
    )

  safe_json_request(
    request = request,
    simplify_data_frame = FALSE,
    error_context = sprintf(
      "Failed to fetch Packet Storm news for section '%s'",
      .packetstorm_scalar(payload$section, default = "unknown")
    )
  )
}

.packetstorm_extract_news_items <- function(response) {
  if (is.null(response)) {
    return(list())
  }

  if (is.list(response$news)) {
    return(response$news)
  }

  if (is.list(response$data) && is.list(response$data$news)) {
    return(response$data$news)
  }

  list()
}

.packetstorm_extract_tag_names <- function(tags) {
  if (is.null(tags) || length(tags) == 0) {
    return(character(0))
  }

  tag_names <- vapply(
    tags,
    function(tag) .packetstorm_scalar(tag$name, default = ""),
    character(1)
  )

  unique(tag_names[nzchar(tag_names)])
}

.packetstorm_parse_news_items <- function(
  response,
  category,
  feed_url,
  api_query,
  include_linked_content = FALSE,
  linked_content_chars = 2500L
) {
  items <- .packetstorm_extract_news_items(response)

  if (length(items) == 0) {
    return(.packetstorm_empty_results())
  }

  rows <- lapply(
    items,
    function(item) {
      meta <- item$meta
      title <- .packetstorm_scalar(meta$title)
      url <- .packetstorm_scalar(meta$url)
      detail <- .packetstorm_scalar(meta$detail)
      posted <- .packetstorm_scalar(meta$posted)
      credit <- .packetstorm_scalar(meta$credit)
      credit_url <- .packetstorm_scalar(meta$credit_url, default = .packetstorm_scalar(meta$homepage))
      tag_names <- .packetstorm_extract_tag_names(meta$tags)
      page_content <- if (isTRUE(include_linked_content) && !is.na(url) && nzchar(url)) {
        .packetstorm_fetch_linked_content(url, max_chars = linked_content_chars)
      } else {
        NA_character_
      }
      description <- .clean_candidate_text(
        paste(
          c(
            detail,
            if (!is.na(credit) && nzchar(credit)) sprintf("Credit: %s", credit),
            if (length(tag_names) > 0) sprintf("Tags: %s", paste(tag_names, collapse = ", "))
          ),
          collapse = "\n\n"
        ),
        max_chars = 1800L
      )

      tibble::tibble(
        source = "packetstorm",
        source_type = "news_api_item",
        category = category,
        title = title,
        url = url,
        guid = dplyr::coalesce(.packetstorm_scalar(item$id), .packetstorm_scalar(meta$id)),
        description = description,
        pub_date = posted,
        feed_url = feed_url,
        credit = credit,
        credit_url = credit_url,
        tag_names = list(tag_names),
        api_query = api_query,
        item_id = dplyr::coalesce(.packetstorm_integer(item$id), .packetstorm_integer(meta$id)),
        page_content = page_content,
        raw_content = .clean_candidate_text(
          paste(
            c(
              title,
              description,
              if (length(tag_names) > 0) paste(tag_names, collapse = ", "),
              page_content
            ),
            collapse = "\n\n"
          ),
          max_chars = max(4500L, as.integer(linked_content_chars))
        )
      )
    }
  )

  dplyr::bind_rows(rows)
}

.packetstorm_collect_manual_entries <- function(
  manual_entries,
  include_linked_content = FALSE,
  linked_content_chars = 2500L,
  feed_url = "manual_http"
) {
  if (is.null(manual_entries) || nrow(manual_entries) == 0) {
    return(.packetstorm_empty_results())
  }

  rows <- lapply(
    seq_len(nrow(manual_entries)),
    function(index) {
      entry <- manual_entries[index, , drop = FALSE]
      article_url <- .packetstorm_scalar(entry$url)
      snapshot <- if (isTRUE(include_linked_content) && !is.na(article_url) && nzchar(article_url)) {
        .packetstorm_fetch_article_snapshot(article_url, max_chars = linked_content_chars)
      } else {
        list(title = NA_character_, page_content = NA_character_)
      }

      entry_title <- .packetstorm_scalar(entry$title, default = snapshot$title)
      page_content <- snapshot$page_content
      tag_values <- .packetstorm_scalar(entry$category)
      manual_tags <- entry$tag_names[[1]]

      if (is.null(manual_tags) || length(manual_tags) == 0) {
        manual_tags <- character(0)
      }

      combined_tags <- unique(c(tag_values[!is.na(tag_values) & nzchar(tag_values)], manual_tags[nzchar(manual_tags)]))
      description <- .clean_candidate_text(
        paste(
          c(
            .packetstorm_scalar(entry$description),
            if (length(combined_tags) > 0) sprintf("Tags: %s", paste(combined_tags, collapse = ", "))
          ),
          collapse = "\n\n"
        ),
        max_chars = 1800L
      )

      tibble::tibble(
        source = "packetstorm",
        source_type = "manual_news_url",
        category = .packetstorm_scalar(entry$category),
        title = entry_title,
        url = article_url,
        guid = article_url,
        description = description,
        pub_date = .packetstorm_scalar(entry$pub_date),
        feed_url = feed_url,
        credit = NA_character_,
        credit_url = NA_character_,
        tag_names = list(combined_tags),
        api_query = NA_character_,
        item_id = NA_integer_,
        page_content = page_content,
        raw_content = .clean_candidate_text(
          paste(
            c(
              entry_title,
              description,
              if (length(combined_tags) > 0) paste(combined_tags, collapse = ", "),
              page_content
            ),
            collapse = "\n\n"
          ),
          max_chars = max(4500L, as.integer(linked_content_chars))
        )
      )
    }
  )

  dplyr::bind_rows(rows)
}

#' Collect PacketStorm raw data
#'
#' @param categories Character vector of Packet Storm News sections.
#' @param pages Number of pages per section to request from the API.
#' @param manual_urls Optional manually curated news/article URLs. When API
#'   credentials are absent, these URLs are fetched directly and routed through
#'   the standard Packet Storm normalization path.
#' @param manual_urls_file Optional path to a UTF-8 text file with one manual
#'   URL entry per line. Supported formats are `url`, `category<TAB>url`, or
#'   `category<TAB>title<TAB>url<TAB>description<TAB>pub_date<TAB>tag1,tag2`.
#' @param output_path Output path for the serialized raw dataset.
#'
#' @return Tibble with Packet Storm raw rows. May be empty when neither the API
#'   secret nor manual URL inputs are available, or when requests fail.
scrape_packetstorm <- function(
  categories = get_default_packetstorm_categories(),
  pages = 1L,
  include_linked_content = FALSE,
  linked_content_chars = 2500L,
  api_url = get_default_packetstorm_api_url(),
  api_secret = NULL,
  manual_urls = get_default_packetstorm_manual_urls(),
  manual_urls_file = get_default_packetstorm_manual_urls_file(),
  output_path = file.path(get_default_data_dir(), "raw_packetstorm.rds")
) {
  categories <- unique(stats::na.omit(as.character(categories)))
  categories <- categories[nzchar(categories)]

  manual_entries <- .packetstorm_read_manual_entries(
    manual_urls = manual_urls,
    manual_urls_file = manual_urls_file
  )

  api_secret <- .packetstorm_api_secret(api_secret)

  if (length(categories) == 0 && nrow(manual_entries) == 0) {
    log_message(
      "PacketStorm collection is disabled because no default categories or manual URLs are configured.",
      level = "INFO"
    )

    packetstorm_raw <- .packetstorm_empty_results()
    save_pipeline_rds(packetstorm_raw, output_path)
    return(packetstorm_raw)
  }

  pages <- as.integer(pages)
  if (is.na(pages) || pages < 1L) {
    pages <- 1L
  }

  if ((length(categories) == 0 || !nzchar(api_secret)) && nrow(manual_entries) > 0) {
    log_message("Collecting raw Packet Storm data from manual HTTP URL list")

    packetstorm_raw <- .packetstorm_collect_manual_entries(
      manual_entries = manual_entries,
      include_linked_content = include_linked_content,
      linked_content_chars = linked_content_chars,
      feed_url = if (nzchar(.packetstorm_scalar(manual_urls_file, default = ""))) {
        .packetstorm_scalar(manual_urls_file)
      } else {
        "PACKETSTORM_URLS"
      }
    )

    save_pipeline_rds(packetstorm_raw, output_path)
    log_message(sprintf("Saved %s Packet Storm rows to %s", nrow(packetstorm_raw), output_path))
    return(packetstorm_raw)
  }

  if (length(categories) == 0) {
    log_message(
      "PacketStorm collection is disabled because no default categories are configured.",
      level = "INFO"
    )

    packetstorm_raw <- .packetstorm_empty_results()
    save_pipeline_rds(packetstorm_raw, output_path)
    return(packetstorm_raw)
  }

  if (!nzchar(api_secret)) {
    log_message(
      "Packet Storm collection is disabled because neither PACKETSTORM_API_SECRET nor manual Packet Storm URL inputs are configured.",
      level = "INFO"
    )

    packetstorm_raw <- .packetstorm_empty_results()
    save_pipeline_rds(packetstorm_raw, output_path)
    return(packetstorm_raw)
  }

  log_message("Collecting raw Packet Storm data")

  rows <- lapply(
    categories,
    function(category) {
      dplyr::bind_rows(lapply(
        seq_len(pages),
        function(page) {
          payload <- list(area = "news", section = category, page = as.integer(page), output = "json")
          api_query <- sprintf("area=news&section=%s&page=%s&output=json", category, page)

          tryCatch(
            {
              response <- .packetstorm_request(payload, api_url = api_url, api_secret = api_secret)
              .packetstorm_parse_news_items(
                response = response,
                category = category,
                feed_url = api_url,
                api_query = api_query,
                include_linked_content = include_linked_content,
                linked_content_chars = linked_content_chars
              )
            },
            error = function(error) {
              log_message(
                sprintf(
                  "Packet Storm section '%s' page '%s' is unavailable: %s",
                  category,
                  page,
                  conditionMessage(error)
                ),
                level = "WARN"
              )
              .packetstorm_empty_results()
            }
          )
        }
      ))
    }
  )

  packetstorm_raw <- dplyr::bind_rows(rows)
  save_pipeline_rds(packetstorm_raw, output_path)
  log_message(sprintf("Saved %s Packet Storm rows to %s", nrow(packetstorm_raw), output_path))
  packetstorm_raw
}