library(testthat)
library(unpackr)

expect_not_exist <- function(..., .env=NULL) {
  qs <- rlang::enquos(...)
  for (q in qs) {
    expect_false(
      exists(rlang::as_string(rlang::quo_squash(q)),
             envir = if (!is.null(.env)) .env else rlang::quo_get_env(q)))
  }
}

tsa <- function(expr1, expr2) {
  expr1 <- rlang::enexpr(expr1)
  expr2 <- rlang::enexpr(expr2)
  local({
    AA <- "oga"
    f <- function() {
      BB <- "ogb"
      function() {
        CC <- "ogc"
        eval(expr1)
        CC
      }
    }
    g <- f()
    CVAL <- g()
    eval(expr2)
  })
}

same_errors <- function(expr, fn, og_fn) {
  assign(deparse(substitute(fn)), fn)
  q <- rlang::enexpr(expr)
  err1 <- rlang::eval_tidy(expect_error(!!q))
  assign(deparse(substitute(fn)), og_fn)
  err2 <- rlang::eval_tidy(expect_error(!!q))
  expect_equal(err1, err2)
}
same_results <- function(expr, fn, og_fn) {
  assign(deparse(substitute(fn)), fn)
  q <- rlang::enexpr(expr)
  res1 <- rlang::eval_tidy(expect_success(!!q))
  assign(deparse(substitute(fn)), og_fn)
  res2 <- rlang::eval_tidy(expect_success(!!q))
  expect_equal(res1, res2)
}
global_sibling_env <- function() {
  rlang::child_env(
    rlang::env_parent(
      globalenv()
    ))
}


test_check("unpackr")
