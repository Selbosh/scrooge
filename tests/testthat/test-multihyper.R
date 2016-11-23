library(scrooge)

context('Multivariate hypergeometric distribution')

test_that("Sampling all balls simply returns original urns",
          expect_equal(rmultihyper(citations, sum(citations)), citations)
)

test_that("Sampling zero balls returns an array of zeroes",
          expect_equal(rmultihyper(citations, 0), matrix(0, nrow(citations), ncol(citations), dimnames = dimnames(citations)))
)
