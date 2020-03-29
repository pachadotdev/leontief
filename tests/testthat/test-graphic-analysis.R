# multiplier_product_matrix ----

test_that("multiplier_product_matrix works with correct dimensions", {
  set.seed(200100)
  L <- matrix(rnorm(100), nrow = 10)
  out <- multiplier_product_matrix(L)
  expect_equal(dim(out), c(10,10))
})

test_that("multiplier_product_matrix fails with incorrect dimensions (non-square X)", {
  set.seed(200100)
  L <- matrix(rnorm(110), nrow = 10, ncol = 11)
  expect_error(multiplier_product_matrix(L))
})
