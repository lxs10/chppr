consumer <- function(key, secret, challenge = FALSE, order = FALSE,
                     youth = FALSE, training = FALSE, bid = FALSE) {
  structure(
    list(
      key = key,
      secret = secret,
      challenge = challenge,
      order = order,
      youth = youth,
      training = training,
      bid = bid
    ),
    class = "consumer"
  )
}

token <- function(token, secret) {
  structure(list(token = token, secret = secret), class = "token")
}

oauth_url <- function() {
  list(
    request_token = "https://chpp.hattrick.org/oauth/request_token.ashx",
    authorize = "https://chpp.hattrick.org/oauth/authorize.aspx",
    authenticate = "https://chpp.hattrick.org/oauth/authenticate.aspx",
    access_token = "https://chpp.hattrick.org/oauth/access_token.ashx",
    check_token = "https://chpp.hattrick.org/oauth/check_token.ashx",
    invalidate_token = "https://chpp.hattrick.org/oauth/invalidate_token.ashx",
    chppxml = "http://chpp.hattrick.org/chppxml.ashx"
  )
}

split_url <- function(url) {
  url <- unlist(strsplit(url, "\\?"))
  base <- url[1]
  if (length(url) == 2) {
    query <- strsplit(unlist(strsplit(url[2], "&")), "=")
    query <- as.list(stats::setNames(sapply(query, `[`, 2), sapply(query, `[`, 1)))
  } else {
    query <- NULL
  }
  list(base = base, query = query)
}

oauth_encode <- function(x) {
  encode <- function(x) {
    x <- unlist(strsplit(as.character(x), ""))
    bad <- grep("[^A-Za-z0-9_.~-]", x)
    x[bad] <- sapply(x[bad], function(x) paste0("%", toupper(charToRaw(x))))
    paste0(x, collapse = "")
  }
  sapply(x, encode)
}

oauth_parameters <- function(url, consumer, token, verifier, callback) {
  url <- split_url(url)
  parameters <- list(
    oauth_callback = callback,
    oauth_consumer_key = consumer$key,
    oauth_nonce = paste(sample(c(letters, LETTERS, 0:9), 10, TRUE), collapse = ""),
    oauth_signature_method = "HMAC-SHA1",
    oauth_timestamp = as.integer(Sys.time()),
    oauth_token = token$token,
    oauth_verifier = verifier,
    oauth_version = "1.0"
  )
  parameters <- parameters[sapply(parameters, function(x) !is.null(x))]
  string <- c(url$query, parameters)
  string <- stats::setNames(oauth_encode(string), oauth_encode(names(string)))
  string <- string[order(names(string))]
  string <- paste0(names(string), "=", string, collapse = "&")
  key <- paste0(oauth_encode(consumer$secret), "&", oauth_encode(token$secret))
  string <- paste0("GET", "&", oauth_encode(url$base), "&", oauth_encode(string))
  parameters$oauth_signature <- httr::hmac_sha1(key, string)
  parameters[order(names(parameters))]
}

oauth_header <- function(url, consumer, token = NULL, verifier = NULL,
                         callback = NULL) {
  parameters <- oauth_parameters(url, consumer, token, verifier, callback)
  authorization <- paste0(
    "OAuth ",
    paste0(
      oauth_encode(names(parameters)), "=\"", oauth_encode(parameters), "\"",
      collapse = ", "
    )
  )
  httr::add_headers(Authorization = authorization)
}

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
