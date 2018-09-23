context("OAuth")
library(httr)
library(rvest)
library(xml2)

test_that("Authorization", {
  consumer <- consumer(
    Sys.getenv("CHPPR_CONSUMER_KEY"), Sys.getenv("CHPPR_CONSUMER_SECRET")
  )
  request_token <- request_token(consumer)
  url <- authorization_url(consumer, request_token)
  session <- html_session(url)
  form <- set_values(
    html_form(session)[[1]],
    txtUsername = Sys.getenv("CHPPR_USERNAME"),
    txtPassword = Sys.getenv("CHPPR_PASSWORD")
  )
  session <- submit_form(session, form)
  content <- content(session$response)
  verifier <- html_text(xml_nodes(content, "pre"))
  expect_identical(
    access_token(consumer, request_token, verifier),
    token(Sys.getenv("CHPPR_ACCESS_TOKEN"), Sys.getenv("CHPPR_ACCESS_TOKEN_SECRET"))
  )
  consumer <- consumer(
    Sys.getenv("CHPPR_CONSUMER_KEY"), Sys.getenv("CHPPR_CONSUMER_SECRET"), TRUE
  )
  request_token <- request_token(consumer)
  url <- authorization_url(consumer, request_token)
  expect_match(
    url, "https://chpp.hattrick.org/oauth/authorize.aspx?oauth_token=",
    fixed = TRUE
  )
})

test_that("CHPP XML", {
  consumer <- consumer(
    Sys.getenv("CHPPR_CONSUMER_KEY"), Sys.getenv("CHPPR_CONSUMER_SECRET")
  )
  access_token <- token(
    Sys.getenv("CHPPR_ACCESS_TOKEN"), Sys.getenv("CHPPR_ACCESS_TOKEN_SECRET")
  )
  chppxml("file=managercompendium", consumer, access_token) %>%
    read_xml() %>%
    xml_find_first("UserID") %>%
    xml_text() %>%
    expect_identical("12495930")
})

test_that("Response errors", {
  expect_error(request_token(consumer("bad_key", "bad_secret")), "401")
  expect_error(
    access_token(
      consumer("bad_key", "bad_secret"),
      token("bad_token", "bad_secret"),
      "bad_verifier"
    ),
    "401"
  )
  expect_error(
    chppxml(
      "bad_query",
      consumer("bad_key", "bad_secret"),
      token("bad_token", "bad_secret")
    ),
    "401"
  )
})

test_that("Printing", {
  consumer <- consumer(
    Sys.getenv("CHPPR_CONSUMER_KEY"), Sys.getenv("CHPPR_CONSUMER_SECRET")
  )
  expect_output(print(consumer))
  token <- token(
    Sys.getenv("CHPPR_ACCESS_TOKEN"), Sys.getenv("CHPPR_ACCESS_TOKEN_SECRET")
  )
  expect_output(print(token))
})
