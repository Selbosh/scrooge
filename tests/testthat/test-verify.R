library(scrooge)

context("Approximate equality of vectors")

test_that("0.33333333333333 is roughly one third", {
  expect_true(0.33333333333333 %==% (1/3))
  expect_false(0.3 %==% (1/3))
})

test_that("Two vectors with element-wise differences of 1e-15 are approximately equal", {
  expect_true((1:5 + 1e-15) %==% 1:5)
  expect_false((1:5 + 1e-5) %==% 1:5)
})

test_that("Prefix operator verify() works on vectors", {
  expect_true(verify(1:5 + 1e-15, 1:5))
  expect_false(verify(1:5 + 1e-5, 1:5))
})

test_that("<= and >= are both TRUE iff a == b", {
  set.seed(1)
  for (i in 1:100) {
    a <- runif(1)
    b <- runif(1)
    expect_equal((a %>=% b) & (a %<=% b), a %==% b)
  }
})



