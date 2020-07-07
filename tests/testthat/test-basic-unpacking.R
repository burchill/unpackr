test_that("RHS %,% preserves length of values (for two values)", {
  expect_equal(
    "A" %,% 22.1,
    list("A", 22.1))
  expect_equal(
    "A" %,% list(22.1),
    list("A", list(22.1)))
  expect_equal(
    list("A") %,% list(22.1),
    list(list("A"), list(22.1)))
  expect_equal(
    list("A") %,% 22.1,
    list(list("A"), 22.1))
})
test_that("RHS %,% preserves length of values (for two values)", {
  expect_equal(
    "A" %,% list(22.1) %,% list(TRUE),
    list("A" , list(22.1) , list(TRUE)))
  expect_equal(
    list("A") %,% 22.1 %,% list(Inf),
    list(list("A") , 22.1 , list(Inf)))
  expect_equal(
    -Inf %,% NA_character_ %,% "X",
    list(-Inf , NA_character_ , "X"))
  expect_equal(
    list("A") %,% 22.1,
    list(list("A"), 22.1))
})




test_that("RHS %,% can handle subcalls and variables", {
  expect_equal(
    list("A") %,% (TRUE || FALSE) %,% paste0("Z","X"),
    list(list("A"), TRUE, "ZX"))
  A <- "A"
  B <- "B"
  expect_equal(
    list("A") %,% A %,% list(A),
    list(list("A"), "A", list("A")))
  expect_equal(
    A %,% B,
    list(A, B))
})

expect_not_exist <- function(...) {
  qs <- rlang::enquos(...)
  for (q in qs) {
    expect_false(
      exists(rlang::as_string(rlang::quo_squash(q)),
             envir = rlang::quo_get_env(q)))
  }
}

test_that("Basic LHS %,% with <-", {
  local({
    AA %,% BB %,% CC <- 1:3
    expect_equal(list(AA, BB, CC), list(1, 2, 3))
  })

  local({
    AA <- BB <- CC <- "X"
    AA %,% BB %,% CC <- 1:3
    expect_equal(list(AA, BB, CC), list(1, 2, 3))
  })

  local({
    AA %,% CC <- list(1, 2)
    expect_equal(list(AA, CC), list(1, 2))
  })


  expect_not_exist(AA, BB, CC)


})



