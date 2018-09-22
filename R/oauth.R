request_token <- function(consumer, callback = "oob") {
  url <- oauth_url()$request_token
  response <- httr::GET(url, oauth_header(url, consumer, callback = callback))
  content <- httr::content(response, "text", "text/html", "UTF-8")
  token <- sapply(strsplit(unlist(strsplit(content, "&"))[1:2], "="), `[`, 2)
  token(token[1], token[2])
}

authorization_url <- function(consumer, request_token) {
  url <- paste0(oauth_url()$authorize, "?oauth_token=", request_token$token)
  scope <- c(
    "manage_challenges",
    "set_matchorder",
    "manage_youthplayers",
    "set_training",
    "place_bid"
  )
  scope <- scope[unlist(consumer[3:7])]
  if (length(scope)) url <- paste0(url, "&scope=", paste(scope, collapse = ","))
  url
}

access_token <- function(consumer, request_token, verifier) {
  url <- oauth_url()$access_token
  response <- httr::GET(url, oauth_header(url, consumer, request_token, verifier))
  content <- httr::content(response, "text", "text/html", "UTF-8")
  token <- sapply(strsplit(unlist(strsplit(content, "&")), "="), `[`, 2)
  token(token[1], token[2])
}

chppxml <- function(query, consumer, access_token) {
  url <- paste0(oauth_url()$chppxml, "?", query)
  response <- httr::GET(url, oauth_header(url, consumer, access_token))
  content <- httr::content(response, "text", "text/xml", "UTF-8")
  content
}
