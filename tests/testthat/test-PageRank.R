library(scrooge)

context("Citation data")

test_that("Sample citation data loads with correct values",
  expect_equal(citations[1:5], c(43, 0, 9, 0, 1))
)

context("PageRank computation")

test_that("PageRank yields correct rank order for 47x47 citation data",
  expect_equal(order(PageRank(citations))[1:5], c(42, 41, 13, 40, 37))
)
