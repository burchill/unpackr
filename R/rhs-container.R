move_over <- function(lhs, rhs) {
  if (is_rhs_container(lhs))
    add_to_rhs(lhs, rhs)
  else
    add_to_rhs(rhs_enclose(lhs), rhs)
}
assert_rhs <- function(x) {
  if (!is_rhs_container(x))
    stop("Expecting a 'rhs-container' object")
}
rhs_enclose <- function(x, extra_l = TRUE) {
  if (extra_l)  x <- list(x)
  structure(list(x), class = "rhs-container")
}
rhs_from_val <- function(x) {
  rhs_enclose(x, extra_l = FALSE)
}
add_to_rhs <- function(rc, x) {
  assert_rhs(rc)
  rhs_enclose(append(rc[[1]], x), extra_l = FALSE)
}
is_rhs_container <- function(x) inherits(x, "rhs-container")
rhs_len <- function(x) {
  assert_rhs(x)
  length(x[[1]])
}
rhs_sub <- function(x, start, diff) {
  base::`<-`(`<-`, base::`<-`)
  assert_rhs(x)
  len <- rhs_len(x)
  if (diff < 0)
    getOption("default_empty_val", NULL)
  else if (diff == 0)
    x[[1]][[start]]
  else
    x[[1]][start:(start+diff)]
}
`print.rhs-container` <- function(..., warn = TRUE) {
  if (warn)
    warning(
      "Values on the righthand side of an ",
      "assignment separated by `%,% produce an ",
      "'rhs-container' object internally. If you are ",
      "seeing this, it generally means something ",
      "has gone wrong. To fix this, ", rhs_ooo(),
      ", and ", assignment_suggestion(), ".",
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
    "'rhs-container' objects are internal and should ",
    "never be converted (i.e., into ", type, "). ",
    "This may be due to R's unintuitive order of operations: ",
    rhs_ooo(), " or ", assignment_suggestion(), "."
  )
}
rhs_ooo <- function(capped = FALSE) {
  paste0(if (capped) "T" else "t", "ry adding ",
         "parentheses around the calls separated by %,%")
}
assignment_suggestion <- function(capped = FALSE) {
  paste0(if (capped) "M" else "m", "ake sure ",
         "assignment is being done via the `<-`, ",
         "`=`, or `<<-` operators exported from unpackr")
}
