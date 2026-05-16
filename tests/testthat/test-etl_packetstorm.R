expect_equal <- testthat::expect_equal
expect_match <- testthat::expect_match

test_that("get_default_packetstorm_categories is disabled by default", {
  categories <- get_default_packetstorm_categories()

  testthat::expect_type(categories, "character")
  testthat::expect_length(categories, 0)
})

test_that("scrape_packetstorm returns empty rows when PacketStorm is disabled", {
  result <- scrape_packetstorm(output_path = tempfile(fileext = ".rds"))

  testthat::expect_equal(nrow(result), 0)
})

test_that("scrape_packetstorm can ingest manual article URLs", {
  page_file <- tempfile(fileext = ".html")
  writeLines(
    c(
      "<html><head><title>Packet Tool</title></head><body>",
      "<h1>Packet Tool</h1>",
      "<p>Post-exploitation utility for credential access.</p>",
      "</body></html>"
    ),
    page_file,
    useBytes = TRUE
  )

  result <- scrape_packetstorm(
    include_linked_content = TRUE,
    manual_urls = page_file,
    output_path = tempfile(fileext = ".rds")
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$source_type[[1]], "manual_news_url")
  testthat::expect_match(result$page_content[[1]], "credential access", fixed = TRUE)
  testthat::expect_match(result$raw_content[[1]], "Packet Tool", fixed = TRUE)
})