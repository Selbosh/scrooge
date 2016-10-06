library(scrooge)

context("Approximate equality of vectors")

test_that("0.3333333 is roughly one third",
  expect_true(0.3333333 %=% (1/3))
)

test_that("Two vectors with element-wise differences of 1e-6 are approximately equal",
  expect_true((1:5 + 1e-6) %=% 1:5)
)

test_that("Prefix operator verify() works on vectors",
  expect_true(verify(1:5 + 1e-6, 1:5))
)
