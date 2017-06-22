context("Journal profiles")

cites6 <- citations[1:6, 1:6]
lst <- list(`1` = 'AoS', `2` = 'Bern', `3` = c('AmS', 'AISM', 'ANZS', 'BioJ'))
vec <- c(3, 3, 1, 3, 2, 3)

ig6_multi <- igraph::graph_from_adjacency_matrix(t(cites6), weighted = NULL)
ig6_weighted <- igraph::graph_from_adjacency_matrix(t(cites6), weighted = TRUE)

ig_multi <- igraph::graph_from_adjacency_matrix(t(citations), weighted = NULL)
ig_weighted <- igraph::graph_from_adjacency_matrix(t(citations), weighted = TRUE)

test_that("Journal profiles are probabilities", {
  expect_true(all(cprofile(citations) >= 0))
  expect_true(all(cprofile(citations) <= 1))
  expect_true(all(colSums(cprofile(citations)) == 1))
})

test_that("Journal profiles have same dimension as citations", {
  expect_equal(dim(cites6), dim(cprofile(cites6)))
  expect_equal(dim(citations), dim(cprofile(citations)))
  expect_equal(dimnames(cites6), dimnames(cprofile(cites6)))
  expect_equal(dimnames(citations), dimnames(cprofile(citations)))
})

test_that("Profiles for igraph objects are consistent", {
  expect_equivalent(as.matrix(cprofile(ig_multi)), cprofile(citations))
  expect_equal(cprofile(ig_multi), cprofile(ig_weighted))
})

context("Community profiles")

test_that("Community profiles are probabilities", {
  expect_true(all(community_profiles(cites6, lst) >= 0))
  expect_true(all(community_profiles(cites6, lst) <= 1))
  expect_true(all(colSums(community_profiles(cites6, lst)) == 1))
})

test_that("Community profile dimensions are correct", {
  expect_equal(dim(community_profiles(cites6, lst)), c(6, 3))
  expect_equal(rownames(community_profiles(cites6, lst)), rownames(cites6))
  expect_equal(colnames(community_profiles(cites6, lst)), names(lst))
})

test_that("Community profiles are the same for equivalent list or vector community spec", {
  expect_equal(community_profiles(cites6, lst), community_profiles(cites6, vec))
})

context("Distances from journal profiles to convex hulls of community profiles")

test_that("Single-journal communities lie within convex hull", {
  expect_equal(as.vector(nearest_point('AoS', cites6, lst)$value), 0)
  expect_equal(as.vector(nearest_point('AoS', cites6, vec)$value), 0)
  expect_equal(as.vector(nearest_point('Bern', cites6, lst)$value), 0)
  expect_equal(as.vector(nearest_point('Bern', cites6, vec)$value), 0)
})

test_that("Distances to convex hull are non-negative", {
  expect_gte(as.vector(nearest_point('AmS', cites6, lst)$value), 0)
  expect_gte(as.vector(nearest_point('AISM', cites6, lst)$value), 0)
  expect_gte(as.vector(nearest_point('ANZS', cites6, lst)$value), 0)
  expect_gte(as.vector(nearest_point('BioJ', cites6, lst)$value), 0)
})

test_that("Coordinates of nearest point are a convex combination of communities", {
  expect_equal(sum(nearest_point('AmS', cites6, lst)$solution), 1)
  expect_equal(sum(nearest_point('AoS', cites6, lst)$solution), 1)
  expect_true(all(nearest_point('AISM', cites6, lst)$solution > -1e-15))
  expect_true(all(nearest_point('Bern', cites6, lst)$solution > -1e-15))
})

test_that("Distances are consistent between igraph and matrix input", {
  expect_equal(nearest_point('AmS', cites6, lst), nearest_point('AmS', ig6_multi, lst))
  expect_equal(nearest_point('AmS', cites6, lst), nearest_point('AmS', ig6_weighted, lst))
  expect_equal(nearest_point('AoS', cites6, lst), nearest_point('AoS', ig6_multi, lst))
  expect_equal(nearest_point('AoS', cites6, lst), nearest_point('AoS', ig6_weighted, lst))
})
