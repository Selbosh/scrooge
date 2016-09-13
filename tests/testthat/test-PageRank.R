library(mypackage)

context("Citation data")

test_that("Sample citation data loads with correct values",
  expect_equal(citations[1], 714)
)

context("PageRank computation")

test_that("PageRank yields correct rank order for 4x4 citation data",
  expect_equal(order(PageRank(citations)), c(2,4,3,1))
)
