#' OAuth Consumer
#'
#' Create a consumer.
#'
#' @param key Consumer key.
#' @param secret Consumer secret.
#' @param challenge Manage challenges?
#' @param order Set match orders?
#' @param youth Manage youth players?
#' @param training Set training?
#' @param bid Place bids?
#'
#' @return An object of class \code{"consumer"}.
#' @export
#'
#' @examples
#' # consumer key and secret below are for illustrative purposes only
#' my_consumer <- consumer(
#'   "WdGMxbWypO3baqGSkrZGtX",
#'   "Bp7dG0GVFJNkAUPT6rHuptmlt1JRqJVIC46ElPHxejf"
#' )
#' # ask for permission to manage challenges
#' my_consumer <- consumer(
#'   "WdGMxbWypO3baqGSkrZGtX",
#'   "Bp7dG0GVFJNkAUPT6rHuptmlt1JRqJVIC46ElPHxejf",
#'   challenge = TRUE
#' )
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

#' @export
print.consumer <- function(x, ...) {
  cat("Consumer key:         ", x$key, "\n", sep = "")
  cat("Consumer secret:      ", x$secret, "\n", sep = "")
  cat("Manage challenges:    ", x$challenge, "\n", sep = "")
  cat("Set match orders:     ", x$order, "\n", sep = "")
  cat("Manage youth players: ", x$youth, "\n", sep = "")
  cat("Set training:         ", x$training, "\n", sep = "")
  cat("Place bids:           ", x$bid, "\n", sep = "")
}

#' OAuth Token
#'
#' Create a token.
#'
#' @param token Token.
#' @param secret Token secret.
#'
#' @return An object of class \code{"token"}.
#' @export
#'
#' @examples
#' # token and token secret below are for illustrative purposes only
#' my_token <- token("pCJS5wFvFfJWVhtY", "irhfsbr7C8c3OwtT")
token <- function(token, secret) {
  structure(list(token = token, secret = secret), class = "token")
}

#' @export
print.token <- function(x, ...) {
  cat("Token:        ", x$token, "\n", sep = "")
  cat("Token secret: ", x$secret, "\n", sep = "")
}
