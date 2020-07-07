test_that("Basic LHS %,% with <<-", {
  tsa(
    AA %,% BB <<- 1:2,
    {
      expect_equal(AA, 1)
      expect_equal(get("BB", envir = environment(g), inherits = FALSE), 2)
    }
  )
  tsa(
    AA %,% BB %,% CC <<- 1:3,
    {
      expect_equal(AA, 1)
      expect_equal(get("BB", envir = environment(g), inherits = FALSE), 2)
      expect_equal(CVAL, "ogc")
      expect_equal(get("CC", envir=globalenv(), inherits = FALSE), 3)
      rm(CC, envir = globalenv())
    }
  )

  tsa(
    AA %,% BB %,% CC <<- list(1:3, 3:6, 6:9),
    {
      expect_equal(AA, 1:3)
      expect_equal(get("BB", envir = environment(g), inherits = FALSE), 3:6)
      expect_equal(CVAL, "ogc")
      expect_equal(get("CC", envir=globalenv(), inherits = FALSE), 6:9)
      rm(CC, envir = globalenv())
    }
  )
  expect_not_exist(AA, BB, CC)
})



