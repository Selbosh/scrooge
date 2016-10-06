library(scrooge)

context('Quasi-symmetry models')

test_that("Scroogefactor retrieves true scaling vector from quasi-symmetric matrix", {
  Q <- rquasisymmetric(20)
  a <- attr(Q, 'a') # True scaling vector
  SF <- Scroogefactor(as.matrix(Q), alpha = 1, sort = FALSE)
  expect_true(all.equal(SF, a))
  })

context('Bradley-Terry model fitting')

test_that("BradleyTerry2 and glm both return same parameter estimates",
  expect_equivalent(BT2(citations), BTscores(citations))
  )
