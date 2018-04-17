# Use readRDS to view reference data sets
#   e.g. readRDS("tests/testthat/sl_test")

library(econullnetr)
context("Bipartite_stats")

# Dummy nullnet object for unit testing
s.1 <- list()
s.1$obs.interactions <- read.csv(system.file("testdata", "obs_data.csv",
                                             package = "econullnetr"))
s.1$rand.data <- read.csv(system.file("testdata", "sim_data.csv",
                                      package = "econullnetr"))
s.1$n.iterations <- 100
class(s.1) <- "nullnet"


# Check error message for significance level outside 0-1.
test_that("Basic error warnings",{
  expect_error(bipartite_stats(s.1, signif.level = 1.1,
                               index.type = "specieslevel",
                               indices = "degree"))
})

# Check that all indices can be handled at all three levels (species, group
#  and network) and that error and warning messages are produced if unsupported
#  indices (e.g. degree distribution) are specified
test_that("Check bipartite specieslevel compatibility",{
  skip_on_cran()
  set.seed(1234)
  expect_error(bipartite_stats(s.1, index.type = "specieslevel",
                               indices = "degree distribution"))
  expect_warning(bipartite_stats(s.1, index.type = "specieslevel",
                                 indices = "ALL", prog.count = FALSE))
  expect_warning(bipartite_stats(s.1, index.type = "grouplevel",
                                 indices = "ALL", prog.count = FALSE))
  expect_error(bipartite_stats(s.1, index.type = "networklevel",
                               indices = "topology"))
  expect_warning(bipartite_stats(s.1, index.type = "networklevel",
                                 indices = "ALL", prog.count = FALSE))
})


test_that("Consistent outputs of the bipartite statistics at all 3 levels",{
  skip_on_cran()
  set.seed(1234)
  expect_equal_to_reference(bipartite_stats(s.1, index.type = "specieslevel",
                                            indices = "ALLBUTD",
                                            prog.count = FALSE), "sl_test")
  expect_equal_to_reference(bipartite_stats(s.1, index.type = "grouplevel",
                                            indices = "ALLBUTDD",
                                            prog.count = FALSE), "gl_test")
  expect_equal_to_reference(bipartite_stats(s.1, index.type = "networklevel",
                                            indices = "ALLBUTDD",
                                            prog.count = FALSE), "nl_test")
})

