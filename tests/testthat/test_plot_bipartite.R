library(econullnetr)
context("Plot_bipartite")

# Dummy nullnet object for unit testing
s.1 <- list()
s.1$obs.interactions <- read.csv(system.file("testdata", "obs_data.csv",
                                             package = "econullnetr"))
s.1$rand.data <- read.csv(system.file("testdata", "sim_data.csv",
                                      package = "econullnetr"))
s.1$n.iterations <- 100
class(s.1) <- "nullnet"

# List to control spp orders (for testing compatibility with plotweb > sequence)
spp.ord <- list()
spp.ord$seq.high <- c("B", "A")
spp.ord$seq.low <- c("Res.2", "Res.1", "Res.3", "Res.4", "Res.5", "Res.6",
                     "Res.7", "Res.8", "Res.9", "Res.10")


# Check that error is generated if significance level is outside 0-1
test_that("Basic error warnings",{
  expect_error(generate_edgelist(s.1, signif.level = 0))
})


# Generate network plots for different significant thresholds, colour schemes
#  and using many of the optional arguments to 'plotweb' to test compatibility
test_that("Bipartite plotting", {
  skip_on_cran()
  pl1 <- function() plot_bipartite(s.1, signif.level = 0.9)
  pl2 <- function() plot_bipartite(s.1, signif.level = 0.95)
  pl3 <- function() plot_bipartite(s.1, signif.level = 0.99)
  pl4 <- function() plot_bipartite(s.1, signif.level = 0.95,
                                   edge.cols = c("#E9A3C9", "#F7F7F7", "#A1D76A"))
  pl5 <- function() plot_bipartite(s.1, signif.level = 0.95,
                                   labsize = 1.2, ybig = 1.1, y.width.low = .05,
                                   y.width.high = .2, arrow = "up",
                                   col.high = "grey", sequence = spp.ord,
                                   text.rot = 90)

# Use the shiny app via vdiffr::manage_cases(".") to create new cases
vdiffr::expect_doppelganger("Plot bipartite v1 0.90", pl1)
vdiffr::expect_doppelganger("Plot bipartite v1 0.95", pl2)
vdiffr::expect_doppelganger("Plot bipartite v1 0.99", pl3)
vdiffr::expect_doppelganger("Plot bipartite v2 0.95", pl4)
vdiffr::expect_doppelganger("Plot bipartite v3 0.95", pl5)
})


test_that("Correct handling of zero values",{
  skip_on_cran()
  # c.1 = one column in consumer data = 0; c.2 = one consumer sp has no
  #  interactions recorded; r.2 = r.1, but with res.7 set to 0, creating a
  #  'double zero' (abundance = 0 and no interactions, but recorded in both
  #  consumer and resource data sets)
  c.1 <- read.csv(system.file("testdata", "df4_cons_zero_res.csv",
                              package = "econullnetr"))
  c.2 <- read.csv(system.file("testdata", "df4_cons_zero_cons.csv",
                              package = "econullnetr"))
  r.1 <- read.csv(system.file("testdata", "df1_res.csv",
                              package = "econullnetr"))
  r.2 <- read.csv(system.file("testdata", "df4_res_zero_res.csv",
                              package = "econullnetr"))

  set.seed(1234)
  n.1 <- generate_null_net(c.1, r.1, sims = 100, prog.count = FALSE)
  n.2 <- generate_null_net(c.2, r.1, sims = 100, prog.count = FALSE)
  n.3 <- generate_null_net(c.1, r.2, sims = 100, prog.count = FALSE)

  pl6 <- function() plot_bipartite(n.1, signif.level = 0.95)
  pl7 <- function() plot_bipartite(n.2, signif.level = 0.95)
  pl8 <- function() plot_bipartite(n.3, signif.level = 0.95)

  vdiffr::expect_doppelganger("Plot bipartite unused resource", pl6)
  vdiffr::expect_doppelganger("Plot bipartite consumer no interactions", pl7)
  vdiffr::expect_doppelganger("Plot bipartite unused resource zero abund", pl8)
})
