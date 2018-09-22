context("OAuth")
library(httr)
library(rvest)

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
})
