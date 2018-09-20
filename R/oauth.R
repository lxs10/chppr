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

oauth_parameters <- function(url, consumer, callback, token, verifier) {
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

oauth_header <- function(url, consumer, callback = NULL, token = NULL,
                         verifier = NULL) {
  parameters <- oauth_parameters(url, consumer, callback, token, verifier)
  authorization <- paste0(
    "OAuth ",
    paste0(
      oauth_encode(names(parameters)), "=\"", oauth_encode(parameters), "\"",
      collapse = ", "
    )
  )
  httr::add_headers(Authorization = authorization)
}
