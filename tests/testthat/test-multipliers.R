# output_multiplier ----

test_that("output_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- output_multiplier(L)
  expect_equal(dim(out), c(10, 1))
})

test_that("output_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(output_multiplier(L))
})

# income_multiplier ----

test_that("income_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(10)
  out <- income_multiplier(L, w)
  expect_equal(dim(out), c(10, 1))
})

test_that("income_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  w <- rnorm(10)
  expect_error(income_multiplier(L, w))
})

test_that("income_multiplier fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(11)
  expect_error(income_multiplier(L, w))
})

# employment_multiplier ----

test_that("employment_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  e <- rnorm(10)
  out <- employment_multiplier(L, e)
  expect_equal(dim(out), c(10, 1))
})

test_that("employment_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  e <- rnorm(10)
  expect_error(employment_multiplier(L, e))
})

test_that("employment_multiplier fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  e <- rnorm(11)
  expect_error(employment_multiplier(L, e))
})
