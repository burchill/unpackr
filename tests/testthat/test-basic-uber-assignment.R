test_that("`<<-` assignment: basic", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))

  sub_env <- rlang::env(a = 1, aprime = 2)
  subsub_env <- rlang::child_env(sub_env)
  local({
    expect_silent(`<<-`(a, "test"))
    expect_silent(base::`<<-`(aprime, "test"))
    expect_identical(a, aprime)
    expect_false(exists("a", inherits = FALSE))
    expect_false(exists("aprime", inherits = FALSE))
  }, envir = subsub_env)
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
})
test_that("`<<-` assignment: basic (string var names)", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))

  sub_env <- rlang::env(a = 1, aprime = 2)
  subsub_env <- rlang::child_env(sub_env)
  local({
    expect_silent(`<<-`("a", "test"))
    expect_silent(base::`<<-`("aprime", "test"))
    expect_identical(a, aprime)
    expect_false(exists("a", inherits = FALSE))
    expect_false(exists("aprime", inherits = FALSE))
  }, envir = subsub_env)
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
})



test_that("`<<-` assignment: in global env if not in parents", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
  sub_env2 <- rlang::env()
  subsub_env2 <- rlang::child_env(sub_env2)
  local({
    expect_silent(`<<-`(a, "test"))
    expect_silent(base::`<<-`(aprime, "test"))
    expect_identical(a, aprime)
    expect_false(exists("a", inherits = FALSE))
    expect_false(exists("aprime", inherits = FALSE))
  }, envir = subsub_env2)

  expect_true(exists("a",      envir = globalenv()))
  expect_true(exists("aprime", envir = globalenv()))
  rm(a, aprime, envir = globalenv())
})
test_that("`<<-` assignment: in global env if not in parents (string var names)", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
  sub_env2 <- rlang::env()
  subsub_env2 <- rlang::child_env(sub_env2)
  local({
    expect_silent(`<<-`("a", "test"))
    expect_silent(base::`<<-`("aprime", "test"))
    expect_identical(a, aprime)
    expect_false(exists("a", inherits = FALSE))
    expect_false(exists("aprime", inherits = FALSE))
  }, envir = subsub_env2)

  expect_true(exists("a",      envir = globalenv()))
  expect_true(exists("aprime", envir = globalenv()))
  rm(a, aprime, envir = globalenv())
})


test_that("`<<-` assignment: basic (but vars exist in current env)", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))

  sub_env <- rlang::env(a = 1, aprime = 2)
  subsub_env <- rlang::child_env(sub_env)
  local({
    base::`<-`(a, "bad")
    base::`<-`(aprime, "alsobad")
    expect_silent(`<<-`(a, "test"))
    expect_silent(base::`<<-`(aprime, "test"))
    expect_identical(a, "bad")
    expect_identical(aprime, "alsobad")
    expect_identical(
      get("a", envir = rlang::env_parent(), inherits = FALSE),
      get("aprime", envir = rlang::env_parent(), inherits = FALSE)
    )
  }, envir = subsub_env)
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
})

test_that("`<<-` assignment: basic (but vars exist in current env; string var names)", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))

  sub_env <- rlang::env(a = 1, aprime = 2)
  subsub_env <- rlang::child_env(sub_env)
  local({
    base::`<-`(a, "bad")
    base::`<-`(aprime, "alsobad")
    expect_silent(`<<-`("a", "test"))
    expect_silent(base::`<<-`("aprime", "test"))
    expect_identical(a, "bad")
    expect_identical(aprime, "alsobad")
    expect_identical(
      get("a", envir = rlang::env_parent(), inherits = FALSE),
      get("aprime", envir = rlang::env_parent(), inherits = FALSE)
    )
  }, envir = subsub_env)
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
})

test_that("`<<-` assignment: in global env if not in parents (but vars exist in current env)", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
  sub_env2 <- rlang::env()
  subsub_env2 <- rlang::child_env(sub_env2)
  local({
    base::`<-`(a, "bad")
    base::`<-`(aprime, "alsobad")
    expect_silent(`<<-`(a, "test"))
    expect_silent(base::`<<-`(aprime, "test"))
    expect_identical(a, "bad")
    expect_identical(aprime, "alsobad")
    expect_identical(
      get("a", envir = globalenv(), inherits = FALSE),
      get("aprime", envir = globalenv(), inherits = FALSE)
    )
  }, envir = subsub_env2)
  rm(a, aprime, envir = globalenv())
})

test_that("`<<-` assignment: in global env if not in parents (but vars exist in current env; string var names)", {
  expect_false(exists("a",      envir = globalenv()))
  expect_false(exists("aprime", envir = globalenv()))
  sub_env2 <- rlang::env()
  subsub_env2 <- rlang::child_env(sub_env2)
  local({
    base::`<-`(a, "bad")
    base::`<-`(aprime, "alsobad")
    expect_silent(`<<-`("a", "test"))
    expect_silent(base::`<<-`("aprime", "test"))
    expect_identical(a, "bad")
    expect_identical(aprime, "alsobad")
    expect_identical(
      get("a", envir = globalenv(), inherits = FALSE),
      get("aprime", envir = globalenv(), inherits = FALSE)
    )
  }, envir = subsub_env2)
  rm(a, aprime, envir = globalenv())
})
