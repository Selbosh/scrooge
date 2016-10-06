library(scrooge)

context("Pseudocounts for paired comparison models")

test_that("Pseudocount with damping factor of 1 returns original matrix",
  expect_equivalent(pseudocount(citations, alpha = 1), citations)
)

test_that("B-T model applied to quasi-symmetric matrix with pseudocounts is equivalent to damped Scrooge", {
  Q <- rquasisymmetric(20)
  Q_ps <- pseudocount(Q, alpha = 0.5)
  expect_gt(cor(BTscores(as.matrix(Q_ps)), Scroogefactor(Q, alpha = 0.5)), 0.9995)
})
