#' Get default security RSS feeds
#'
#' @return Named character vector of default RSS feed URLs.
get_default_feeds <- function() {
  character(0)
}

.rss_empty_results <- function() {
  tibble::tibble(
    source = character(),
    source_type = character(),
    feed_title = character(),
    feed_url = character(),
    item_title = character(),
    item_link = character(),
    item_description = character(),
    item_pub_date = character(),
    item_guid = character(),
    item_categories = list(),
    page_content = character(),
    raw_content = character()
  )
}

.rss_fetch_linked_content <- function(item_link, source_name, max_chars = 2500L) {
  if (identical(source_name, "cisa_advisories")) {
    return(NA_character_)
  }

  tryCatch(
    .extract_linked_page_text(item_link, max_chars = max_chars),
    error = function(error) {
      log_message(
        sprintf("Linked RSS content unavailable for %s: %s", item_link, conditionMessage(error)),
        level = "WARN"
      )
      NA_character_
    }
  )
}

.rss_node_text <- function(node, xpath) {
  target <- xml2::xml_find_first(node, xpath)

  if (inherits(target, "xml_missing")) {
    return(NA_character_)
  }

  value <- xml2::xml_text(target)
  ifelse(nzchar(value), value, NA_character_)
}

.rss_node_attr <- function(node, xpath, attr) {
  target <- xml2::xml_find_first(node, xpath)

  if (inherits(target, "xml_missing")) {
    return(NA_character_)
  }

  value <- xml2::xml_attr(target, attr)
  ifelse(!is.na(value) && nzchar(value), value, NA_character_)
}

.rss_parse_document <- function(
  xml_document,
  source_name,
  feed_url,
  include_linked_content = FALSE,
  linked_content_chars = 2500L
) {
  root_name <- xml2::xml_name(xml2::xml_root(xml_document))

  if (identical(root_name, "rss")) {
    feed_title <- .rss_node_text(xml2::xml_root(xml_document), ".//channel/title")
    items <- xml2::xml_find_all(xml_document, ".//channel/item")

    if (length(items) == 0) {
      return(.rss_empty_results())
    }

    rows <- lapply(
      items,
      function(item) {
        description <- .rss_node_text(item, "./description")
        item_link <- .rss_node_text(item, "./link")
        page_content <- if (isTRUE(include_linked_content)) {
          .rss_fetch_linked_content(item_link, source_name = source_name, max_chars = linked_content_chars)
        } else {
          NA_character_
        }
        categories <- xml2::xml_find_all(item, "./category")

        tibble::tibble(
          source = source_name,
          source_type = "rss_item",
          feed_title = feed_title,
          feed_url = feed_url,
          item_title = .rss_node_text(item, "./title"),
          item_link = item_link,
          item_description = description,
          item_pub_date = .rss_node_text(item, "./pubDate"),
          item_guid = .rss_node_text(item, "./guid"),
          item_categories = list(xml2::xml_text(categories)),
          page_content = page_content,
          raw_content = .clean_candidate_text(
            paste(c(description, page_content), collapse = "\n\n"),
            max_chars = max(4000L, as.integer(linked_content_chars))
          )
        )
      }
    )

    return(dplyr::bind_rows(rows))
  }

  if (identical(root_name, "feed")) {
    feed_title <- .rss_node_text(xml2::xml_root(xml_document), "./title")
    entries <- xml2::xml_find_all(xml_document, ".//*[local-name()='entry']")

    if (length(entries) == 0) {
      return(.rss_empty_results())
    }

    rows <- lapply(
      entries,
      function(entry) {
        summary <- .rss_node_text(entry, "./*[local-name()='summary']")
        content <- .rss_node_text(entry, "./*[local-name()='content']")
        item_link <- .rss_node_attr(entry, "./*[local-name()='link'][1]", "href")
        page_content <- if (isTRUE(include_linked_content)) {
          .rss_fetch_linked_content(item_link, source_name = source_name, max_chars = linked_content_chars)
        } else {
          NA_character_
        }
        categories <- xml2::xml_find_all(entry, "./*[local-name()='category']")

        tibble::tibble(
          source = source_name,
          source_type = "atom_entry",
          feed_title = feed_title,
          feed_url = feed_url,
          item_title = .rss_node_text(entry, "./*[local-name()='title']"),
          item_link = item_link,
          item_description = dplyr::coalesce(summary, content),
          item_pub_date = dplyr::coalesce(
            .rss_node_text(entry, "./*[local-name()='updated']"),
            .rss_node_text(entry, "./*[local-name()='published']")
          ),
          item_guid = .rss_node_text(entry, "./*[local-name()='id']"),
          item_categories = list(xml2::xml_attr(categories, "term")),
          page_content = page_content,
          raw_content = .clean_candidate_text(
            paste(c(summary, content, page_content), collapse = "\n\n"),
            max_chars = max(4000L, as.integer(linked_content_chars))
          )
        )
      }
    )

    return(dplyr::bind_rows(rows))
  }

  log_message(
    sprintf("Unsupported RSS/Atom root element '%s' for %s", root_name, feed_url),
    level = "WARN"
  )

  .rss_empty_results()
}

#' Fetch and normalize security RSS feeds
#'
#' @param feed_urls Named or unnamed character vector of feed URLs.
#' @param output_path Output path for the serialized raw dataset.
#'
#' @return Tibble with normalized RSS items.
fetch_security_feeds <- function(
  feed_urls = get_default_feeds(),
  include_linked_content = FALSE,
  linked_content_chars = 2500L,
  output_path = file.path(get_default_data_dir(), "raw_rss.rds")
) {
  if (length(feed_urls) == 0) {
    log_message(
      "RSS collection is disabled because no default feeds are configured.",
      level = "INFO"
    )

    rss_raw <- .rss_empty_results()
    save_pipeline_rds(rss_raw, output_path)
    return(rss_raw)
  }

  source_names <- names(feed_urls)

  if (is.null(source_names)) {
    source_names <- rep(NA_character_, length(feed_urls))
  }

  log_message("Collecting raw RSS data")

  rows <- lapply(
    seq_along(feed_urls),
    function(index) {
      feed_url <- unname(feed_urls[[index]])
      source_name <- source_names[[index]]

      if (is.na(source_name) || !nzchar(source_name)) {
        source_name <- sprintf("feed_%s", index)
      }

      tryCatch(
        {
          xml_document <- xml2::read_xml(feed_url)
          .rss_parse_document(
            xml_document,
            source_name = source_name,
            feed_url = feed_url,
            include_linked_content = include_linked_content,
            linked_content_chars = linked_content_chars
          )
        },
        error = function(error) {
          log_message(
            sprintf("RSS feed '%s' is unavailable: %s", feed_url, conditionMessage(error)),
            level = "WARN"
          )
          .rss_empty_results()
        }
      )
    }
  )

  rss_raw <- dplyr::bind_rows(rows)
  save_pipeline_rds(rss_raw, output_path)
  log_message(sprintf("Saved %s RSS rows to %s", nrow(rss_raw), output_path))
  rss_raw
}