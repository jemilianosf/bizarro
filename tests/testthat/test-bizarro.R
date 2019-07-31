context("test-bizarro.R")

test_that("letters in string reversed", {
  expect_equal(bizarro("abc"), "cba")
})

test_that("integers & doubles are negated", {
  expect_equal(bizarro(1L), -1L)
  expect_equal(bizarro(1), -1)
})

test_that("logicals are not-ted", {
  expect_equal(bizarro(c(TRUE, FALSE)), c(FALSE, TRUE))
})

test_that("dataframes get bizarro names and columns", {
  df <- data.frame(xyz = "abc", stringsAsFactors = FALSE)
  expect_equal(bizarro(df), data.frame(zyx = "cba", stringsAsFactors = FALSE))
})

test_that("factors get reversed levels", {
  expect_equal(bizarro(factor(c(1,2,3,4))), factor(c(4,3,2,1),levels = c(4,3,2,1)))
})
