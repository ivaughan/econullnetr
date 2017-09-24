library(econullnetr)
context("Plot_preferences")

# Dummy nullnet object for unit testing + load files for controlling plot order
s.1 <- list()
s.1$obs.interactions <- read.csv(system.file("testdata", "obs_data.csv",
                                             package = "econullnetr"))
s.1$rand.data <- read.csv(system.file("testdata", "sim_data.csv",
                                      package = "econullnetr"))
s.1$n.iterations <- 100
class(s.1) <- "nullnet"
ro <- read.csv(system.file("testdata", "df1_res_order.csv", package = "econullnetr"))
ro.err1 <- read.csv(system.file("testdata", "df1_res_order_error.csv",
                                package = "econullnetr"))


# Check error messages: i) significance level is outside 0-1, ii) invalid
#  'bar.type' value, & iii) resource names match when a 'res.order' data frame
#  is specified.
test_that("Basic error warnings",{
  expect_error(plot_preferences(s.1, node = "A", signif.level = 1.1))
  expect_error(plot_preferences(s.1, node = "A", type = "height"))
  expect_error(plot_preferences(s.1, node = "A", res.order = ro[-1, ]))
  expect_error(plot_preferences(s.1, node = "A", res.order = ro.err1))
})


test_that("Check appearance of plot",{
  skip_on_cran()
  pl1 <- function() plot_preferences(s.1, node = "A", signif.level = 0.95,
                                     style = "dots", type = "counts")
  pl2 <- function() plot_preferences(s.1, node = "B", signif.level = 0.95,
                                     style = "dots", type = "counts")
  pl3 <- function() plot_preferences(s.1, node = "A", style = "dots",
                                     type = "SES")
  pl4 <- function() plot_preferences(s.1, node = "B", style = "dots",
                                     type = "SES")
  pl5 <- function() plot_preferences(s.1, node = "A", signif.level = 0.95,
                                     style = "bars", type = "counts")
  pl6 <- function() plot_preferences(s.1, node = "B", signif.level = 0.95,
                                     style = "bars", type = "counts")
  pl7 <- function() plot_preferences(s.1, node = "A", style = "bars",
                                     type = "SES")
  pl8 <- function() plot_preferences(s.1, node = "B", style = "bars",
                                     type = "SES")

  # Use the shiny app via vdiffr::manage_cases(".") to create new cases
  vdiffr::expect_doppelganger("Plot prefs dots A 0.95", pl1)
  vdiffr::expect_doppelganger("Plot prefs dots B 0.95", pl2)
  vdiffr::expect_doppelganger("Plot prefs dots A SES", pl3)
  vdiffr::expect_doppelganger("Plot prefs dots B SES", pl4)
  vdiffr::expect_doppelganger("Plot prefs bars A 0.95", pl5)
  vdiffr::expect_doppelganger("Plot prefs bars B 0.95", pl6)
  vdiffr::expect_doppelganger("Plot prefs bars A SES", pl7)
  vdiffr::expect_doppelganger("Plot pref bars B SES", pl8)
})
