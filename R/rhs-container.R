move_over <- function(lhs, rhs) {
  if (is_rhs_container(lhs))
    add_to_rhs(lhs, rhs)
  else
    add_to_rhs(rhs_enclose(lhs), rhs)
}
is_rhs_starred <- function(x) {
  assert_rhs(x)
  attr(x, "star")
}
assert_rhs <- function(x) {
  if (!is_rhs_container(x))
    stop("Expecting a 'rhs-container' object")
}
rhs_enclose <- function(x, extra_l = TRUE, star = FALSE) {
  if (extra_l)  x <- list(x)
  structure(list(x), star = star, class = "rhs-container")
}
rhs_from_val <- function(x) {
  rhs_enclose(x, extra_l = FALSE, star = FALSE)
}
add_to_rhs <- function(rc, x) {
  assert_rhs(rc)
  rhs_enclose(
    append(rc[[1]], x), extra_l = FALSE,
    star   = is_rhs_starred(rc))
}
is_rhs_container <- function(x) inherits(x, "rhs-container")
rhs_len <- function(x) {
  assert_rhs(x)
  length(x[[1]])
}
rhs_head <- function(x, n, rm_star = FALSE) {
  assert_rhs(x)
  star <- !rm_star && is_rhs_starred(x)
  if (n==1)
    x[[1]][[1]]
  else
    rhs_enclose(x[[1]][1:n], FALSE, star = star)
}
rhs_tail <- function(x, n) {
  assert_rhs(x)
  len <- rhs_len(x)
  if (n > len)
    getOption("default_empty_val", NULL)
  else if (n == len)
    x[[1]][[n]]
  else
    x[[1]][n:len]
}
`print.rhs-container` <- function(..., warn = TRUE) {
  if (warn)
    warning(
      "Values on the righthand side of an ",
      "assignment separated by `%,% produce an ",
      "'rhs-container' object internally. If you are ",
      "seeing this, it generally means something ",
      "has gone wrong. Make sure that calls ",
      "between `%,%` have parentheses around them.",
      immediate. = TRUE
    )
  print.default(...)
}
`as.logical.rhs-container` <- function(...) cannot_convert("logicals")
`as.character.rhs-container` <- function(...) cannot_convert("characters")
`as.numeric.rhs-container` <- function(...) cannot_convert("numerics")
`as.integer.rhs-container` <- function(...) cannot_convert("integers")
`as.double.rhs-container` <- function(...) cannot_convert("doubles")
`as.data.frame.rhs-container` <- function(...) cannot_convert("data.frames")
cannot_convert <- function(type) {
  stop(
    "'rhs-container' objects cannot be converted ",
    "into ", type, ". ",
    "Make sure that values separated by `%,%` ",
    "on the righthand side of assignments are ",
    "properly surrounded by parentheses."
  )
}
