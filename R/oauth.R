#' OAuth Request Token
#'
#' Get a request token.
#'
#' @param consumer A consumer, created by \code{\link{consumer}}.
#' @param callback Callback URL, \code{"oob"} by default.
#'
#' @return A request token, an object of class \code{"token"}.
#' @export
#'
#' @examples
#' \dontrun{
#' # consumer key and secret below are for illustrative purposes only
#' my_consumer <- consumer(
#'   "WdGMxbWypO3baqGSkrZGtX",
#'   "Bp7dG0GVFJNkAUPT6rHuptmlt1JRqJVIC46ElPHxejf"
#' )
#' my_request_token <- request_token(my_consumer)
#' }
request_token <- function(consumer, callback = "oob") {
  url <- oauth_url()$request_token
  response <- httr::GET(url, oauth_header(url, consumer, callback = callback))
  content <- httr::content(response, "text", "text/html", "UTF-8")
  if (response$status_code != 200) {
    html <- xml2::read_html(content, "UTF-8")
    title <- xml2::xml_text(xml2::xml_find_first(html, "head/title"))
    stop(title)
  }
  token <- sapply(strsplit(unlist(strsplit(content, "&"))[1:2], "="), `[`, 2)
  token(token[1], token[2])
}

#' OAuth Authorization URL
#'
#' Get an authorization URL.
#'
#' @param consumer A consumer, created by \code{\link{consumer}}.
#' @param request_token A request token, created by \code{\link{request_token}}.
#'
#' @return An authorization URL.
#' @export
#'
#' @examples
#' \dontrun{
#' # consumer key and secret below are for illustrative purposes only
#' my_consumer <- consumer(
#'   "WdGMxbWypO3baqGSkrZGtX",
#'   "Bp7dG0GVFJNkAUPT6rHuptmlt1JRqJVIC46ElPHxejf"
#' )
#' my_request_token <- request_token(my_consumer)
#' my_authorization_url <- authorization_url(my_consumer, my_request_token)
#' browseURL(my_authorization_url)
#' }
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

#' OAuth Access Token
#'
#' Get an access token.
#'
#' @param consumer A consumer, created by \code{\link{consumer}}.
#' @param request_token A request token, created by \code{\link{request_token}}.
#' @param verifier A verification code, received after authorization.
#'
#' @return An access token, an object of class \code{"token"}.
#' @export
#'
#' @examples
#' \dontrun{
#' # consumer key and secret below are for illustrative purposes only
#' my_consumer <- consumer(
#'   "WdGMxbWypO3baqGSkrZGtX",
#'   "Bp7dG0GVFJNkAUPT6rHuptmlt1JRqJVIC46ElPHxejf"
#' )
#' my_request_token <- request_token(my_consumer)
#' my_authorization_url <- authorization_url(my_consumer, my_request_token)
#' browseURL(my_authorization_url)
#' # verification code below is for illustrative purposes only
#' my_access_token <- access_token(my_consumer, my_request_token, "jHuNpFdZOB8JXNzR")
#' }
access_token <- function(consumer, request_token, verifier) {
  url <- oauth_url()$access_token
  response <- httr::GET(url, oauth_header(url, consumer, request_token, verifier))
  content <- httr::content(response, "text", "text/html", "UTF-8")
  if (response$status_code != 200) {
    html <- xml2::read_html(content, "UTF-8")
    title <- xml2::xml_text(xml2::xml_find_first(html, "head/title"))
    stop(title)
  }
  token <- sapply(strsplit(unlist(strsplit(content, "&")), "="), `[`, 2)
  token(token[1], token[2])
}

#' CHPP XML Files
#'
#' Get CHPP XML files.
#'
#' @param query A query string, e.g. \code{"file=teamdetails&version=3.4"}.
#' @param consumer A consumer, created by \code{\link{consumer}}.
#' @param access_token An access token, created by \code{\link{access_token}}.
#'
#' @return A CHPP XML file.
#' @export
#'
#' @examples
#' \dontrun{
#' # consumer key and secret below are for illustrative purposes only
#' my_consumer <- consumer(
#'   "WdGMxbWypO3baqGSkrZGtX",
#'   "Bp7dG0GVFJNkAUPT6rHuptmlt1JRqJVIC46ElPHxejf"
#' )
#' my_request_token <- request_token(my_consumer)
#' my_authorization_url <- authorization_url(my_consumer, my_request_token)
#' browseURL(my_authorization_url)
#' # verification code below is for illustrative purposes only
#' my_access_token <- access_token(my_consumer, my_request_token, "jHuNpFdZOB8JXNzR")
#' my_chppxml <- chppxml("file=teamdetails&version=3.4", my_consumer, my_access_token)
#' }
chppxml <- function(query, consumer, access_token) {
  url <- paste0(oauth_url()$chppxml, "?", query)
  response <- httr::GET(url, oauth_header(url, consumer, access_token))
  if (response$status_code != 200) {
    content <- httr::content(response, "text", "text/html", "UTF-8")
    html <- xml2::read_html(content, "UTF-8")
    title <- xml2::xml_text(xml2::xml_find_first(html, "head/title"))
    stop(title)
  }
  content <- httr::content(response, "text", "text/xml", "UTF-8")
  content
}
