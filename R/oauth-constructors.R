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

print.consumer <- function(x, ...) {
  cat("Consumer key:         ", x$key, "\n", sep = "")
  cat("Consumer secret:      ", x$secret, "\n", sep = "")
  cat("Manage challenges:    ", x$challenge, "\n", sep = "")
  cat("Set match orders:     ", x$order, "\n", sep = "")
  cat("Manage youth players: ", x$youth, "\n", sep = "")
  cat("Set training:         ", x$training, "\n", sep = "")
  cat("Place bids:           ", x$bid, "\n", sep = "")
}

token <- function(token, secret) {
  structure(list(token = token, secret = secret), class = "token")
}

print.token <- function(x, ...) {
  cat("Token:        ", x$token, "\n", sep = "")
  cat("Token secret: ", x$secret, "\n", sep = "")
}
