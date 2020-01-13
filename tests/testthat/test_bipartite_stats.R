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


# Creat reference bipartite_stats outputs
# 1. Species level
l1 <- list()
l1[[1]] <- data.frame(Species = c("A", "B"), Observed = c(10, 5),
                      Null = c(9.64, 7.50), Lower.CL = c(8.475, 5.000),
                      Upper.CL = c(10, 9), Test = c("ns", "ns"),
                      SES = c(0.6646941, -2.4751238))
l1[[1]]$Test <- as.character(l1[[1]]$Test)

l1[[2]] <- data.frame(Species = c("Res.1", "Res.10", "Res.2", "Res.3",
                                  "Res.4", "Res.5", "Res.6", "Res.7",
                                  "Res.8", "Res.9"),
                      Observed = c(2, 1, 1, 2, 1, 2, 1, 2, 1, 2),
                      Null = c(2.000000, 1.970000, 1.900000, 1.717172,
                               1.456522, 1.730000, 2.000000, 1.618557,
                               1.696970, 1.373626),
                      Lower.CL = c(2.000, 1.475, 1.000, 1.000, 1.000, 1.000,
                                   2.000, 1.000, 1.000, 1.000),
                      Upper.CL = rep(2, 10),
                      Test = c("ns", "Lower", "ns", "ns", "ns", "ns",
                               "Lower", "ns", "ns", "ns"),
                      SES = c(NaN, -5.6577381, -2.9849623, 0.6248061,
                              -0.9115205, 0.6051152, -Inf, 0.7812229,
                              -1.5088962, 1.2876521))
l1[[2]]$Test <- as.character(l1[[2]]$Test)
names(l1)[[1]] <- "higher"
names(l1)[[2]] <- "lower"


l2 <- list()
l2[[1]] <- data.frame(Species = c("A", "B"), Observed = c(8.333333, 1.666667),
                      Null = c(7.421677, 2.358323),
                      Lower.CL = c(6.292542, 1.531228),
                      Upper.CL = c(8.443645, 3.394187), Test = c("ns", "ns"),
                      SES = c(1.589373, -1.420179))
l2[[1]]$Test <- as.character(l2[[1]]$Test)

l2[[2]] <- data.frame(Species = c("Res.1", "Res.10", "Res.2", "Res.3",
                                  "Res.4", "Res.5", "Res.6", "Res.7",
                                  "Res.8", "Res.9"),
                      Observed = c(0.33333333, 0.06666667, 0.06666667,
                                   0.33333333, 0.06666667, 0.33333333,
                                   0.06666667, 0.33333333, 0.06666667,
                                   0.33333333),
                      Null = c(0.79360000, 0.21146667, 0.19560000, 0.10074074,
                               0.05391304, 0.11560000, 0.31440000, 0.08481100,
                               0.09427609, 0.04879121),
                      Lower.CL = c(0.60000000, 0.10666667, 0.07300000,
                                   0.01333333, 0.01333333, 0.04000000,
                                   0.17333333, 0.02666667, 0.01333333,
                                   0.01333333),
                      Upper.CL = c(0.9733333, 0.3333333, 0.3536667, 0.1940000,
                                   0.1393333, 0.2400000, 0.4736667, 0.1866667,
                                   0.2000000, 0.1366667),
                      Test = c("Lower", "Lower", "Lower", "Higher", "ns",
                               "Higher", "Lower", "Higher", "ns", "Higher"),
                      SES = c(-4.0498092, -2.2414323, -1.7444423, 5.1362431,
                              0.3618648, 4.0517805, -2.8243618, 5.4806015,
                              -0.5541260, 8.7249030))
l2[[2]]$Test <- as.character(l2[[2]]$Test)
names(l2)[[1]] <- "higher"
names(l2)[[2]] <- "lower"

sl.res <- list(l1, l2)
names(sl.res) <- c("degree", "species.strength")

# 2. Group level
gl.res <- data.frame(Observed = c(8.7500000, 1.7500000, 2.0868235, 0.4773856),
                     Null = c(9.1050000, 1.9205000, 1.7956946, 0.5119895),
                     Lower.CL = c(7.868750, 1.820000, 1.626484,0.474236 ),
                     Upper.CL = c(9.7500000, 1.9800000, 1.9614262, 0.5419423),
                     Test = c("ns", "Lower", "Higher", "ns"),
                     SES = c(-0.7361558, -3.8390048, 3.5893696, -1.7760717))
gl.res$Test <- as.character(gl.res$Test)
rownames(gl.res) <- c("mean.number.of.links.HL", "mean.number.of.links.LL",
                      "partner.diversity.HL", "partner.diversity.LL")


# 3. Network level
nl.res <- data.frame(Observed = c(0.7500000, 0.4168528),
                     Null = c(0.8765972, 0.3302291),
                     Lower.CL = c(0.7631944, 0.2957805),
                     Upper.CL = c(0.9500000, 0.3732541),
                     Test = c("Lower", "Higher"),
                     SES = c(-2.568718, 4.099562))
nl.res$Test <- as.character(nl.res$Test)
rownames(nl.res) <- c("connectance", "weighted connectance")



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
  expect_equal(bipartite_stats(s.1, index.type = "specieslevel",
               indices = c("degree","species strength"), prog.count = FALSE),
               sl.res, tolerance = 1e-5)
  expect_equal(bipartite_stats(s.1, index.type = "grouplevel",
                               indices = c("mean number of links",
                                           "partner diversity"),
                               prog.count = FALSE), gl.res, tolerance = 1e-5)
  expect_equal(bipartite_stats(s.1, index.type = "networklevel",
                               indices = c("connectance",
                                           "weighted connectance"),
                               prog.count = FALSE), nl.res, tolerance = 1e-5)
})

