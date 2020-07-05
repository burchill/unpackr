give_star <- function(x) {
  assert_lhs(x)
  `attr<-`(x, "star", TRUE)
}
is_lhs_starred <- function(x) {
  assert_lhs(x)
  attr(x, "star")
}
assert_lhs <- function(x) {
  if (!is_lhs_count(x))
    stop("Expecting a 'lhs-count' object")
}
incr <- function(x) {
  if (is_lhs_count(x)) `[[<-`(x, 1, x[[1]]+1) else count()
}
is_lhs_count <- function(x) inherits(x, "lhs-count")
count <- function() structure(list(1), star=FALSE, class = c("lhs-count"))
lhs_expose <- function(x) {
  assert_lhs(x)
  x[[1]]
}
`print.lhs-count` <- function(..., warn = TRUE) {
  if (warn)
    warning(
      "Variables on the lefthand side of an ",
      "assignment separated by `%,% produce an ",
      "'lhs-count' object internally. If you are ",
      "seeing this, it generally means something ",
      "has gone wrong",
      immediate. = TRUE
    )
  print.default(...)
}
