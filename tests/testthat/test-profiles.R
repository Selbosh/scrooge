context("Journal profiles")

cites6 <- citations[1:6, 1:6]
memb <- setNames(c(1, 2, 3, 2, 2, 4), colnames(cites6))
ncommunities <- length(unique(memb))

ig6_multi <- igraph::graph_from_adjacency_matrix(t(cites6), weighted = NULL)
ig6_weighted <- igraph::graph_from_adjacency_matrix(t(cites6), weighted = TRUE)
comms <- igraph::cluster_optimal(ig6_multi)

ig_multi <- igraph::graph_from_adjacency_matrix(t(citations), weighted = NULL)
ig_weighted <- igraph::graph_from_adjacency_matrix(t(citations), weighted = TRUE)

test_that("Journal profiles are probabilities", {
  expect_true(all(cprofile(citations) >= 0))
  expect_true(all(cprofile(citations) <= 1))
  expect_equivalent(colSums(cprofile(citations)), rep.int(1, ncol(citations)))
})

test_that("Journal profiles have same dimension as citations", {
  expect_equal(dim(cites6), dim(cprofile(cites6)))
  expect_equal(dim(citations), dim(cprofile(citations)))
  expect_equal(dimnames(cites6), dimnames(cprofile(cites6)))
  expect_equal(dimnames(citations), dimnames(cprofile(citations)))
})

test_that("Profiles for igraph objects are consistent", {
  expect_equal(cprofile(ig_multi), cprofile(citations))
  expect_equal(cprofile(ig_multi), cprofile(ig_weighted))
})

context("Community profiles")

test_that("Community profiles are probabilities", {
  expect_true(all(community_profile(cites6, memb) >= 0))
  expect_true(all(community_profile(cites6, memb) <= 1))
  expect_true(all.equal(colSums(community_profile(cites6, memb)), rep.int(1, ncommunities), check.names = FALSE))
})

test_that("Community profile dimensions are correct", {
  expect_equal(dim(community_profile(cites6, memb)), c(6, ncommunities))
  expect_equal(rownames(community_profile(cites6, memb)), rownames(cites6))
})

context("Distances from journal profiles to convex hulls of community profiles")

test_that("Single-journal communities lie within convex hull", {
  expect_equal(as.vector(nearest_point('AoS', cites6, memb)$value), 0)
  expect_equal(as.vector(nearest_point('AoS', cites6, comms)$value), 0)
  expect_equal(as.vector(nearest_point('BioJ', cites6, memb)$value), 0)
  expect_equal(as.vector(nearest_point('BioJ', cites6, comms)$value), 0)
})

test_that("Distances to convex hull are non-negative", {
  expect_gte(as.vector(nearest_point('AmS', cites6, memb)$value),  -1e-15)
  expect_gte(as.vector(nearest_point('AISM', cites6, memb)$value), -1e-15)
  expect_gte(as.vector(nearest_point('ANZS', cites6, memb)$value), -1e-15)
  expect_gte(as.vector(nearest_point('BioJ', cites6, memb)$value), -1e-15)
})

test_that("Coordinates of nearest point are a convex combination of communities", {
  expect_equal(sum(nearest_point('AmS', cites6, memb)$solution), 1)
  expect_equal(sum(nearest_point('AoS', cites6, memb)$solution), 1)
  expect_true(all(nearest_point('AISM', cites6, memb)$solution > -1e-15))
  expect_true(all(nearest_point('BioJ', cites6, memb)$solution > -1e-15))
})

test_that("Distances are consistent between igraph and matrix input", {
  expect_equal(nearest_point('AmS', cites6, memb), nearest_point('AmS', ig6_multi, memb))
  expect_equal(nearest_point('AmS', cites6, memb), nearest_point('AmS', ig6_weighted, memb))
  expect_equal(nearest_point('AoS', cites6, memb), nearest_point('AoS', ig6_multi, memb))
  expect_equal(nearest_point('AoS', cites6, memb), nearest_point('AoS', ig6_weighted, memb))
})

test_that("A singleton community's profile should equal its constituent journal profile", {
  # AmS (community 1), AoS (community 3) and BioJ (community 4) are singletons in `memb`.
  expect_equal(cprofile(cites6)[, 'AmS'], community_profile(cites6, memb)[, memb['AmS']])
  expect_equal(cprofile(cites6)[, 'AoS'], community_profile(cites6, memb)[, memb['AoS']])
  expect_equal(cprofile(cites6)[, 'BioJ'], community_profile(cites6, memb)[, memb['BioJ']])
})

test_that("Citation rates within/between singleton communities are unchanged by aggregation", {
  # As above, assuming AmS, AoS and BioJ are singletons.
  expect_equivalent(cprofile(cites6)['AoS', 'AmS'], community_profile(cites6, memb)['AoS', memb['AmS']])
  expect_equivalent(cprofile(cites6)['BioJ', 'AoS'], community_profile(cites6, memb)['BioJ', memb['AoS']])
  expect_equivalent(cprofile(cites6)['AoS', 'AoS'], community_profile(cites6, memb)['AoS', memb['AoS']])
})
