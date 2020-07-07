#' @export
`%,%` <- function(lhs, rhs) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    move_over(lhs, rhs)
  } else {
    inexplic_err(match.call())
  }
}
#' @export
`%,*%` <- function(lhs, rhs) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    stop("Cannot star supplied values")
  } else {
    inexplic_err(match.call())
  }
}

#' 'Guide rail' exports
#'
#' `packr` exports
#'
#' @export
`%,%<-` <- function(lhs, rhs, value) {
  stop("`%,%<-` is being evaluated literally, which ",
       "should never happen. ", assignment_suggestion(TRUE), ".")
}
#' @export
`%,*%<-` <- function(lhs, rhs, value) {
  stop("`%,*%<-` is being evaluated literally, which ",
       "should never happen. ", assignment_suggestion(TRUE), ".")
}
inexplic_err <- function(call) {
  stop(
    "`", deparse(call[[1]]), "` should not be evaluated ",
    "in this context (literally, on the lefthand side ",
    "of assignment). ", assignment_suggestion(TRUE), ".",
    call. = FALSE)
}
