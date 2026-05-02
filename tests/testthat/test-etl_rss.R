test_that("get_default_feeds returns named feed URLs", {
  feeds <- get_default_feeds()

  expect_type(feeds, "character")
  expect_true(length(feeds) >= 2)
  expect_false(is.null(names(feeds)))
})

test_that("fetch_security_feeds parses RSS feeds", {
  feed_file <- tempfile(fileext = ".xml")
  xml <- paste0(
    "<?xml version='1.0' encoding='UTF-8'?>",
    "<rss version='2.0'><channel><title>Example Feed</title>",
    "<item><title>Item One</title><link>https://example.com/1</link>",
    "<description>Desc 1</description><guid>id-1</guid>",
    "<pubDate>Sat, 04 Apr 2026 12:00:00 GMT</pubDate><category>tag-a</category></item>",
    "</channel></rss>"
  )
  writeLines(xml, feed_file, useBytes = TRUE)

  output_path <- tempfile(fileext = ".rds")
  result <- fetch_security_feeds(c(example = feed_file), output_path = output_path)

  expect_equal(nrow(result), 1)
  expect_equal(result$source[[1]], "example")
  expect_equal(result$item_title[[1]], "Item One")
  expect_true(file.exists(output_path))
})

test_that("fetch_security_feeds parses Atom feeds", {
  feed_file <- tempfile(fileext = ".xml")
  xml <- paste0(
    "<?xml version='1.0' encoding='UTF-8'?>",
    "<feed xmlns='http://www.w3.org/2005/Atom'><title>Example Atom</title>",
    "<entry><title>Atom Item</title><id>atom-1</id>",
    "<updated>2026-04-04T12:00:00Z</updated>",
    "<link href='https://example.com/atom-1'/><summary>Atom desc</summary>",
    "<category term='research'/></entry></feed>"
  )
  writeLines(xml, feed_file, useBytes = TRUE)

  result <- fetch_security_feeds(c(atom = feed_file), output_path = tempfile(fileext = ".rds"))

  expect_equal(nrow(result), 1)
  expect_equal(result$item_title[[1]], "Atom Item")
  expect_equal(result$item_link[[1]], "https://example.com/atom-1")
})

test_that("fetch_security_feeds can enrich entries with linked page content", {
  page_file <- tempfile(fileext = ".html")
  writeLines(
    c(
      "<html><body>",
      "<h1>Exploit Tool</h1>",
      "<p>Utility for lateral movement and credential access.</p>",
      "</body></html>"
    ),
    page_file,
    useBytes = TRUE
  )

  feed_file <- tempfile(fileext = ".xml")
  xml <- paste0(
    "<?xml version='1.0' encoding='UTF-8'?>",
    "<rss version='2.0'><channel><title>Example Feed</title>",
    "<item><title>Item One</title><link>", page_file, "</link>",
    "<description>Desc 1</description><guid>id-1</guid>",
    "<pubDate>Sat, 04 Apr 2026 12:00:00 GMT</pubDate><category>tag-a</category></item>",
    "</channel></rss>"
  )
  writeLines(xml, feed_file, useBytes = TRUE)

  result <- fetch_security_feeds(
    c(example = feed_file),
    include_linked_content = TRUE,
    output_path = tempfile(fileext = ".rds")
  )

  expect_match(result$page_content[[1]], "Utility for lateral movement", fixed = TRUE)
  expect_match(result$raw_content[[1]], "Exploit Tool", fixed = TRUE)
})