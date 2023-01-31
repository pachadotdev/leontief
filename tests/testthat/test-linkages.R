# backward_linkage ----

test_that("backward_linkage works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- backward_linkage(L)
  expect_equal(dim(out), c(10, 1))
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
  expect_equal(dim(out), c(10, 1))
})

test_that("forward_linkage fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(forward_linkage(L))
})

# equilibrium_output ----

test_that("equilibrium_output works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(10)
  out <- equilibrium_output(L, d)
  expect_equal(dim(out), c(10, 1))
})

test_that("equilibrium_output fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  d <- rnorm(10)
  expect_error(equilibrium_output(L, d))
})

test_that("equilibrium_output fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(11)
  expect_error(equilibrium_output(L, d))
})

# power_dispersion ----

test_that("power_dispersion works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- power_dispersion(L)
  expect_equal(dim(out), c(10, 1))
})

test_that("power_dispersion fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(power_dispersion(L))
})

# sensitivity_dispersion ----

test_that("sensitivity_dispersion works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- sensitivity_dispersion(L)
  expect_equal(dim(out), c(10, 1))
})

test_that("sensitivity_dispersion fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(sensitivity_dispersion(L))
})

# power_dispersion_cv ----

test_that("power_dispersion_cv works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- power_dispersion_cv(L)
  expect_equal(dim(out), c(10, 1))
})

test_that("power_dispersion_cv fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(power_dispersion_cv(L))
})

# sensitivity_dispersion_cv ----

test_that("sensitivity_dispersion_cv works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- sensitivity_dispersion_cv(L)
  expect_equal(dim(out), c(10, 1))
})

test_that("sensitivity_dispersion_cv fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(sensitivity_dispersion_cv(L))
})

# multiplier_product_matrix ----

test_that("multiplier_product_matrix works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- multiplier_product_matrix(L)
  expect_equal(dim(out), c(10, 10))
})

test_that("multiplier_product_matrix fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(multiplier_product_matrix(L))
})
