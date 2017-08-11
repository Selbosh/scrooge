context("Profile and community residuals")

dist <- as.dist(1 - cor(citations + t(citations) - diag(diag(citations))))
clusters <- cutree(hclust(dist), h = 0.6)

test_that("Vector of predicted citations is equivalent to from a null glm", {
  Bka_predicted <- fitted_citations('Bka', citations, clusters)
  Bka_residuals <- profile_residuals(Bka_predicted, citations[, 'Bka'])
  Bka_null_glm <- glm(citations[, 'Bka'] ~ 0 + offset(log(Bka_predicted)), family = 'poisson')

  expect_true(Bka_predicted %==% fitted(Bka_null_glm))
  expect_true(Bka_residuals %==% resid(Bka_null_glm, type = 'pearson'))
})
