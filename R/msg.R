# Helpers for messaging ---------------------------------------------------


#' Show a colorized message for note.
#' @param msg The message to be shown
#' @export
note <- function(msg) {
  msg <- paste("\u2746", msg, "\n")
  cat(crayon::cyan(msg))
}


#' Show a colorized successful message
#' @param msg The message to be shown
#' @export
good <- function(msg) {
  msg <- paste("\u2713", msg, "\n")
  cat(crayon::bold(crayon::green(msg)))
}


#' Show a colorized failure message
#' @param msg The message to be shown
#' @export
bad <- function(msg) {
  msg <- paste("\u2718", msg, "\n")
  cat(crayon::red(crayon::bold(msg)))
}


#' Show a colorized warning message
#' @param msg The message to be shown
#' @export
warn <- function(msg) {
  msg <- paste("\u26A0", msg, "\n")
  cat(crayon::yellow(crayon::bold(msg)))
}


#' Show a todo task with a bullet point
#' @param msg The message to be shown
#' @export
todo <- function(msg) {
  msg <- paste(crayon::red("\u2022"), crayon::magenta(crayon::bold(msg)), "\n")
  cat(msg)
}
