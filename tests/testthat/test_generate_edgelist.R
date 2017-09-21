library(econullnetr)
context("Generate_edgelist")

# Dummy nullnet object for unit testing
s.1 <- list()
s.1$obs.interactions <- read.csv(system.file("testdata", "obs_data.csv",
                                             package = "econullnetr"))
s.1$rand.data <- read.csv(system.file("testdata", "sim_data.csv",
                                      package = "econullnetr"))
s.1$n.iterations <- 100
class(s.1) <- "nullnet"


# Check that error is generated if significance level is outside 0-1
test_that("Basic error warnings",{
  expect_error(generate_edgelist(s.1, signif.level = 0))
})


# Check output is consistent with previous runs. Check three different signif
#  levels, an alternative colour scheme and forcing interactions that were not
#  observed to be included in the output.
test_that("Consistent outputs", {
  expect_equal_to_reference(generate_edgelist(s.1, signif.level = 0.9),
                            "el1_090")
  expect_equal_to_reference(generate_edgelist(s.1, signif.level = 0.95),
                            "el1_095")
  expect_equal_to_reference(generate_edgelist(s.1, signif.level = 0.99),
                            "el1_099")
  expect_equal_to_reference(generate_edgelist(s.1, signif.level = 0.95,
                            edge.cols = c("#E9A3C9", "#F7F7F7", "#A1D76A")),
                            "el2_095")
  expect_equal_to_reference(generate_edgelist(s.1, signif.level = 0.95,
                            export.null = TRUE), "el3_95")

})
