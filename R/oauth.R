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
