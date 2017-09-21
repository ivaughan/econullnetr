library(econullnetr)
context("Test_interactions")

# Data sets and basic model runs for unit testing
s.1 <- list()
s.1$obs.interactions <- read.csv(system.file("testdata", "obs_data.csv",
                                             package = "econullnetr"))
s.1$rand.data <- read.csv(system.file("testdata", "sim_data.csv",
                                      package = "econullnetr"))
s.1$n.iterations <- 100
class(s.1) <- "nullnet"
c.1 <- read.csv(system.file("testdata", "df1_cons.csv", package = "econullnetr"))
c.2 <- read.csv(system.file("testdata", "df3_cons.csv", package = "econullnetr"))
c.3 <- read.csv(system.file("testdata", "df1_cons_one_res_zero.csv",
                            package = "econullnetr"))
r.1 <- read.csv(system.file("testdata", "df1_res.csv", package = "econullnetr"))
fbls.1 <- read.csv(system.file("testdata", "df1_fbls.csv",
                               package = "econullnetr"))


# Check that error is generated if significance level is outside 0-1
test_that("Basic error warnings",{
  expect_error(test_interactions(s.1, signif.level = 1.1))
})


# Check the warnings appear when: i) there are >50 interactions between consumer
#  and resource species and ii) <100 iterations of the null model were run
test_that("Type I error warning", {
  skip_on_cran()
  n.1 <- generate_null_net(c.1, r.1, sims = 5, prog.count = FALSE)
  n.2 <- generate_null_net(c.2, r.1, sims = 100, prog.count = FALSE)
  expect_warning(test_interactions(n.1))
  expect_warning(test_interactions(n.2))
})


# Check the table and test results
test_that("Correct outputs", {
  skip_on_cran()
  expect_equal_to_reference(test_interactions(s.1, 0.9), "ti1_090")
  expect_equal_to_reference(test_interactions(s.1, 0.95), "ti1_095")
  expect_equal_to_reference(test_interactions(s.1, 0.99), "ti1_099")
})


# Check that resources that are not consumed, and those identified as forbidden
#   links, are handled correctly
test_that("Correct handling of zero values and forbidden links", {
  skip_on_cran()
  t1 <- test_interactions(generate_null_net(c.3, r.1, sims = 100,
                                            prog.count = FALSE))
  t2 <- test_interactions(generate_null_net(c.1, r.1, sims = 100,
                                            r.weights = fbls.1,
                                            prog.count = FALSE))
  expect_equal(sum(t1[t1$Resource == "Res.3", "Observed"]), 0)
  expect_equal(sum(is.na(t2[t2$Resource == "Res.1" || t2$Resource == "Res.3",
                            "SES"])), 4)
})
