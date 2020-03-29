# equilibrium_output ----

test_that("equilibrium_output works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(10)
  out <- equilibrium_output(L,d)
  expect_equal(dim(out), c(10,1))
})

test_that("equilibrium_output fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  d <- rnorm(10)
  expect_error(equilibrium_output(L,d))
})

test_that("equilibrium_output fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(11)
  expect_error(equilibrium_output(L,d))
})

# output_multiplier ----

test_that("output_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- output_multiplier(L)
  expect_equal(dim(out), c(10,1))
})

test_that("output_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(output_multiplier(L))
})

# household_income_multiplier ----

test_that("household_income_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(10)
  out <- household_income_multiplier(L,w)
  expect_equal(dim(out), c(10,1))
})

test_that("household_income_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  W <- rnorm(10)
  expect_error(household_income_multiplier(L,w))
})

test_that("household_income_multiplier fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(11)
  expect_error(household_income_multiplier(L,w))
})

# income_multiplier ----

test_that("income_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(10)
  out <- income_multiplier(L,w)
  expect_equal(dim(out), c(10,1))
})

test_that("income_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  w <- rnorm(10)
  expect_error(income_multiplier(L,w))
})

test_that("income_multiplier fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(11)
  expect_error(income_multiplier(L,w))
})

# employment_multiplier ----

test_that("employment_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  e <- rnorm(10)
  out <- employment_multiplier(L,e)
  expect_equal(dim(out), c(10,1))
})

test_that("employment_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  e <- rnorm(10)
  expect_error(employment_multiplier(L,e))
})

test_that("employment_multiplier fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  e <- rnorm(11)
  expect_error(employment_multiplier(L,e))
})

# input_multiplier ----

test_that("input_multiplier works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- input_multiplier(L)
  expect_equal(dim(out), c(10,1))
})

test_that("input_multiplier fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(input_multiplier(L))
})

# backward_linkage ----

test_that("backward_linkage works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- backward_linkage(L)
  expect_equal(dim(out), c(10,1))
})

test_that("backward_linkage fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(backward_linkage(L))
})

# forward_linkage ----

test_that("forward_linkage works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- forward_linkage(L)
  expect_equal(dim(out), c(10,1))
})

test_that("forward_linkage fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(forward_linkage(L))
})

# backward_linkage_cv ----

test_that("backward_linkage_cv works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- backward_linkage_cv(L)
  expect_equal(dim(out), c(10,1))
})

test_that("backward_linkage_cv fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(backward_linkage_cv(L))
})

# forward_linkage_cv ----

test_that("forward_linkage_cv works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- forward_linkage_cv(L)
  expect_equal(dim(out), c(10,1))
})

test_that("forward_linkage_cv fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(forward_linkage_cv(L))
})
