context("Profile and community residuals")

dist <- as.dist(1 - cor(citations + t(citations) - diag(diag(citations))))
clusters <- cutree(hclust(dist), h = 0.6)

test_that("Predicted citations from a given journal are equivalent to from a null glm", {
  Bka_predicted <- fitted_citations('Bka', citations, clusters)
  Bka_residuals <- profile_residuals(Bka_predicted, citations[, 'Bka'])
  Bka_null_glm <- glm(citations[, 'Bka'] ~ 0 + offset(log(Bka_predicted)), family = poisson)

  expect_true(Bka_predicted %==% fitted(Bka_null_glm))
  expect_true(Bka_residuals %==% resid(Bka_null_glm, type = 'pearson'))
})


test_that("Predicted citations for full network are equivalent to from a null glm", {
  all_predicted <- fitted_citations(NULL, citations, clusters)
  all_residuals <- profile_residuals(all_predicted, citations)

  offset <- log(c(all_predicted))
  offset[!is.finite(offset)] <- NA
  all_null_glm <- glm(c(citations) ~ 0, offset = offset, family = poisson, na.action = na.exclude)
  fitted_glm <- fitted(all_null_glm)
  fitted_glm[is.na(fitted_glm)] <- 0

  expect_true(all_predicted %==% fitted_glm)
  expect_equal(sum(c(is.na(all_residuals))), sum(is.na(resid(all_null_glm, type = 'pearson')))) # no. of NAs
  expect_true(all(abs(c(all_residuals) - resid(all_null_glm, type = 'pearson')) <= 1e-7, na.rm = TRUE))
})
