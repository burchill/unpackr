#' @export
`%,%` <- function(lhs, rhs) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    move_over(lhs, rhs)
  } else {
    check_if_symbol(substitute(rhs))
    incr(lhs)
  }
}
#' @export
`%,*%` <- function(lhs, rhs) {
  if (deparse(substitute(lhs)) != "*tmp*") {
    stop("Cannot star supplied values")
  } else {
    if (is_lhs_count(lhs) && is_lhs_starred(lhs))
      stop("Cannot have two starred variables in assignment")
    check_if_symbol(substitute(rhs))
    give_star(incr(lhs))
  }
}
#' @export
`%,%<-` <- function(lhs, rhs, value) {
  check_assignment_style(substitute(lhs), "%,%")
  check_if_symbol(substitute(rhs))
  l <- shared_proc(lhs, rhs, value, is_starred = FALSE)
  assign(deparse(substitute(rhs)), l[[2]], envir = parent.frame())
  l[[1]]
}
#' @export
`%,*%<-` <- function(lhs, rhs, value) {
  check_assignment_style(substitute(lhs), "%,*%")
  check_if_symbol(substitute(rhs))
  l <- shared_proc(lhs, rhs, value, is_starred = TRUE)
  assign(deparse(substitute(rhs)), l[[2]], envir = parent.frame())
  l[[1]]
}

shared_proc <- function(lhs, rhs, value, is_starred = FALSE) {
  first_time <- FALSE
  # This means the rhs wasn't separated and this is the start of assignment
  if (!is_rhs_container(value)) {
    value <- rhs_from_val(value)
    first_time <- TRUE
  }
  starred <- is_starred || is_rhs_starred(value) ||
    (is_lhs_count(lhs) && is_lhs_starred(lhs))
  counter <- if (is_lhs_count(lhs)) lhs_expose(lhs) + 1 else 1
  len <- rhs_len(value)
  issue <- (!starred && counter + 1 != len) || counter > len
  if (first_time && issue)
    stop(counter + 1, " variables receiving assignment, ",
         "but ", len, " values supplied")

  if (is_starred)
    list(rhs_head(value, counter), rhs_tail(value, counter+1))
  else
    list(rhs_head(value, len-1), rhs_tail(value, len))
}
check_if_symbol <- function(x) {
  if (!rlang::is_symbol(x))
    stop("`%,%` can only separate bare variable names ",
         "(not ", deparse(x), ")", call. = FALSE)
}
check_assignment_style <- function(sub_lhs, name) {
  if (deparse(sub_lhs) != "*tmp*")
    stop("`", name, "<-` cannot be called explicitly ",
         "(e.g. '`", name, "<-`(a, b, 1:2)'. Use the ",
         "form: 'a ", name, " b <- 1:2'",
         call. = FALSE)
}
