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

test_that("Basic LHS %,% with =", {
  local({
    AA %,% BB %,% CC = 1:3
    expect_equal(list(AA, BB, CC), list(1, 2, 3))
  })

  local({
    AA = BB = CC = "X"
    AA %,% BB %,% CC = 1:3
    expect_equal(list(AA, BB, CC), list(1, 2, 3))
  })

  local({
    AA %,% CC = list(1, 2)
    expect_equal(list(AA, CC), list(1, 2))
  })

  expect_not_exist(AA, BB, CC)
})


test_that("Basic LHS %,*% with <-", {
  local({
    AA %,% BB %,*% CC <- 1:3
    expect_equal(list(AA, BB, CC), list(1, 2, 3))
  })

  local({
    AA %,*% BB <- 1:2
    expect_equal(list(AA, BB), list(1, 2))
  })

  local({
    AA <- BB <- CC <- "X"
    AA %,% BB %,% CC <- 1:3
    expect_equal(list(AA, BB, CC), list(1, 2, 3))
  })

  local({
    AA %,*% CC <- list(1, 2)
    expect_equal(list(AA, CC), list(1, 2))
  })

  expect_not_exist(AA, BB, CC)
})

test_that("%,*% makes list(1) -> 1 for first variable", {
  local({
    AA %,*% BB <- list(1)
    expect_equal(list(AA, BB), list(1, NULL))
  })
  local({
    AA %,*% BB = list(1)
    expect_equal(list(AA, BB), list(1, NULL))
  })
  AA <- BB <- "X"
  local({
    AA %,*% BB <<- list(1)
    expect_equal(list(AA, BB), list(1, NULL))
  })
  rm(AA, BB)
})

test_that("%,*% with nulls", {
  local({
    AA %,*% BB <- 1
    expect_equal(list(AA, BB), list(1, NULL))
  })
  local({
    AA %,*% BB <- list(c(1,2,3))
    expect_equal(list(AA, BB), list(c(1,2,3), NULL))
  })
  local({
    AA %,% BB %,*% CC <- 1:2
    expect_equal(list(AA, BB, CC), list(1, 2, NULL))
  })
  local({
    AA %,*% BB %,% CC <- 1:2
    expect_equal(list(AA, BB, CC), list(1, NULL, 2))
  })

  expect_not_exist(AA, BB, CC)
})



