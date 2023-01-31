# input_requirement ----

test_that("input_requirement works with correct dimensions", {
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(10)
  out <- input_requirement(X, d)
  expect_equal(dim(out), c(10, 10))
})

test_that("input_requirement fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  X <- matrix(rnorm(110), nrow = 10, ncol = 11)
  d <- rnorm(10)
  expect_error(input_requirement(X, d))
})

test_that("input_requirement fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(11)
  expect_error(input_requirement(X, d))
})

# augmented_input_requirement ----

test_that("augmented_input_requirement works with correct dimensions", {
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(10)
  c <- rnorm(10)
  d <- rnorm(10)
  out <- augmented_input_requirement(X, w, c, d)
  expect_equal(dim(out), c(11, 11))
})

test_that("augmented_input_requirement fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  X <- matrix(rnorm(110), nrow = 10, ncol = 11)
  w <- rnorm(10)
  c <- rnorm(10)
  d <- rnorm(10)
  expect_error(augmented_input_requirement(X, w, c, d))
})

test_that("augmented_input_requirement fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  w <- rnorm(10)
  c <- rnorm(10)
  d <- rnorm(11)
  expect_error(augmented_input_requirement(X, w, c, d))
})

# output_allocation ----

test_that("output_allocation works with correct dimensions", {
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(10)
  out <- output_allocation(X, d)
  expect_equal(dim(out), c(10, 10))
})

test_that("output_allocation fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  X <- matrix(rnorm(110), nrow = 10, ncol = 11)
  d <- rnorm(10)
  expect_error(output_allocation(X, d))
})

test_that("output_allocation fails with incorrect dimensions (d different than dimensions of X)", {
  set.seed(200100)
  X <- matrix(rnorm(100), nrow = 10)
  d <- rnorm(11)
  expect_error(output_allocation(X, d))
})

# leontief_inverse ----

test_that("leontief_inverse works with correct dimensions", {
  set.seed(200100)
  A <- matrix(rnorm(100), nrow = 10)
  out <- leontief_inverse(A)
  expect_equal(dim(out), c(10, 10))
})

test_that("leontief_inverse fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  A <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(leontief_inverse(A))
})
