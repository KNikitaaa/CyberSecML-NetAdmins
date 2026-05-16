.clean_candidate_text <- function(text, max_chars = 4000L) {
  if (is.null(text) || length(text) == 0) {
    return(NA_character_)
  }

  value <- text[[1]]

  if (is.null(value) || is.na(value)) {
    return(NA_character_)
  }

  value <- as.character(value)

  if (!nzchar(value)) {
    return(NA_character_)
  }

  cleaned <- value
  cleaned <- gsub("\r\n?", "\n", cleaned, perl = TRUE)
  cleaned <- gsub("(?s)```.*?```", " ", cleaned, perl = TRUE)
  cleaned <- gsub("(?s)<!--.*?-->", " ", cleaned, perl = TRUE)
  cleaned <- gsub("!\\[[^]]*\\]\\([^)]*\\)", " ", cleaned, perl = TRUE)
  cleaned <- gsub("\\[([^]]+)\\]\\([^)]*\\)", "\\1", cleaned, perl = TRUE)
  cleaned <- gsub("<[^>]+>", " ", cleaned, perl = TRUE)
  cleaned <- gsub("https?://\\S+", " ", cleaned, perl = TRUE)
  cleaned <- gsub("(?m)^#{1,6}\\s*", "", cleaned, perl = TRUE)
  cleaned <- gsub("(?m)^>+\\s*", "", cleaned, perl = TRUE)
  cleaned <- gsub("(?m)^[-*]{3,}$", " ", cleaned, perl = TRUE)
  cleaned <- gsub("(?m)^\\|.*\\|$", " ", cleaned, perl = TRUE)
  cleaned <- gsub("[^[:alnum:][:space:][:punct:]]+", " ", cleaned, perl = TRUE)
  cleaned <- stringr::str_squish(cleaned)

  if (!nzchar(cleaned)) {
    return(NA_character_)
  }

  max_chars <- as.integer(max_chars)

  if (!is.na(max_chars) && max_chars > 0L && nchar(cleaned, type = "chars") > max_chars) {
    return(substr(cleaned, 1L, max_chars))
  }

  cleaned
}

.extract_linked_page_text <- function(url, max_chars = 2500L) {
  if (is.null(url) || length(url) == 0) {
    return(NA_character_)
  }

  page_url <- as.character(url[[1]])

  if (is.na(page_url) || !nzchar(page_url)) {
    return(NA_character_)
  }

  safe_run(
    expr = function() {
      document <- rvest::read_html(page_url)
      removable_nodes <- rvest::html_elements(document, "script, style, noscript, svg")

      if (length(removable_nodes) > 0) {
        xml2::xml_remove(removable_nodes)
      }

      body <- rvest::html_element(document, "body")

      if (length(body) == 0 || inherits(body, "xml_missing")) {
        return(NA_character_)
      }

      .clean_candidate_text(xml2::xml_text(body), max_chars = max_chars)
    },
    error_context = sprintf("Failed to fetch linked page content from %s", page_url)
  )
}