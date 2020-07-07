test_that("Can't explicitly call assignment operators", {
  a <- NULL
  b <- NULL
  expect_error(`%,*%<-`(a, b, 1:2))
  expect_error(`%,%<-`(a, b, 1:2))
})






compare_for_undefined <- function(fn, og_fn) {
  fn <- rlang::enexpr(fn)
  og_fn <- rlang::enexpr(og_fn)
  all_exprs <- rlang::expr({
    same_errors(
      local((!!fn)(a[[1]], "test"),
            rlang::env_parent(globalenv())),
      fn, og_fn)
  })
  rlang::eval_tidy(all_exprs)
}

compare_for_strings <- function(fn, og_fn) {
  fn <- rlang::enexpr(fn)
  og_fn <- rlang::enexpr(og_fn)
  all_exprs <- rlang::expr({
    local({
      (!!fn)("a","test")
      (!!og_fn)("aprime", "test")
      expect_identical(a, aprime)
    },
    global_sibling_env())
  })
  rlang::eval_tidy(all_exprs)
}
compare_for_nonvariables <- function(fn, og_fn) {
  fn <- rlang::enexpr(fn)
  og_fn <- rlang::enexpr(og_fn)
  all_exprs <- rlang::expr({
    same_errors((!!fn)(NA, "test"), fn, og_fn)
    same_errors((!!fn)(NULL, "test"), fn, og_fn)
    same_errors((!!fn)(Inf, "test"), fn, og_fn)
    same_errors((!!fn)(NaN, "test"), fn, og_fn)
    same_errors((!!fn)(99, "test"), fn, og_fn)
    same_errors((!!fn)(NA[[1]], "test"), fn, og_fn)
    same_errors((!!fn)(NULL[[1]], "test"), fn, og_fn)
    same_errors((!!fn)(Inf[[1]], "test"), fn, og_fn)
    same_errors((!!fn)(NaN[[1]], "test"), fn, og_fn)
    same_errors((!!fn)("string"[[1]], "test"), fn, og_fn)
    same_errors((!!fn)(c("A"), "test"), fn, og_fn)
    same_errors((!!fn)(c("A")[[1]], "test"), fn, og_fn)
  })
  rlang::eval_tidy(all_exprs)
}


test_that("Basic assigning to non-variable objects ", {
  compare_for_nonvariables(`<-`, base::`<-`)
  compare_for_nonvariables(`=`, base::`=`)
  compare_for_nonvariables(`<<-`, base::`<<-`)
})

test_that("Subset assignment to undefined variables", {
  compare_for_undefined(`<-`,  base::`<-`)
  compare_for_undefined(`=`,   base::`=`)
  compare_for_undefined(`<<-`, base::`<<-`)
})

# test_that("Assignment with missing args", {
#   expect_failure(same_errors(`<-`(a), `<-`, base::`<-`))
# })

test_that("Basic assigning to strings", {
  compare_for_strings(`<-`, base::`<-`)
  compare_for_strings(`=`,  base::`=`)
})



