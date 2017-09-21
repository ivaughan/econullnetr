library(econullnetr)
context("Generate_null_net")

# Data sets for unit testing:
# df1 = 2 consumers, 10 resources, no subgroups in the data
# df2 = 2 consumers, 10 resources, 2 subgroups
# m, c and q suffix = multinomial data, counts and quantities
c.1 <- read.csv(system.file("testdata", "df1_cons.csv", package = "econullnetr"))
c.1.m <- read.csv(system.file("testdata", "df1_cons_m.csv", package = "econullnetr"))
c.1.c <- read.csv(system.file("testdata", "df1_cons_c.csv", package = "econullnetr"))
c.1.q <- read.csv(system.file("testdata", "df1_cons_q.csv", package = "econullnetr"))
r.1 <- read.csv(system.file("testdata", "df1_res.csv", package = "econullnetr"))
c.2 <- read.csv(system.file("testdata", "df2_cons.csv", package = "econullnetr"))
r.2 <- read.csv(system.file("testdata", "df2_res.csv", package = "econullnetr"))
c.1.err1 <- read.csv(system.file("testdata", "df1_cons_not_integer.csv",
                                    package = "econullnetr"))
r.1.err1 <- read.csv(system.file("testdata", "df1_res_wrong_order.csv",
                                   package = "econullnetr"))
r.1.warn1 <- read.csv(system.file("testdata", "df1_res_zero_abundance.csv",
                                    package = "econullnetr"))
fbls.1 <- read.csv(system.file("testdata", "df1_fbls.csv",
                               package = "econullnetr"))
fbls.1.err1 <-read.csv(system.file("testdata", "df1_fbls_wrong_order.csv",
                                   package = "econullnetr"))
fbls.1.err2 <-read.csv(system.file("testdata", "df1_fbls_wrong_range.csv",
                                   package = "econullnetr"))

test_that("Data sets are compatible", {
  skip_on_cran()
  # Different number of resources between 'consumers' and 'resources'
  expect_error(generate_null_net(c.1, r.1[, c(1:3, 5:10)], sims = 5,
                                 prog.count = FALSE))
  # Column names in 'consumers' and 'resources' in different orders
  expect_error(generate_null_net(c.1, r.1.err1, sims = 5,
                                 prog.count = FALSE))
  # Column names in 'r.weights' in different order from 'resources' and 'consumers'
  expect_error(generate_null_net(c.1, r.1, sims = 5, r.weights = fbls.1.err1,
                                 prog.count = FALSE))
  # Too few resources in 'r.weights'
  expect_error(generate_null_net(c.1, r.1, sims = 5, prog.count = FALSE,
                                 r.weights = fbls.1[, c(1:3, 5:11)]))
  # Too few consumers in 'r.weights'
  expect_error(generate_null_net(c.1, r.1, sims = 5, r.weights = fbls.1[-1, ],
                                 prog.count = FALSE))
  # Only one of r.samples/c.samples present
  expect_error(generate_null_net(c.2[, 2:12], r.2[, 2:11], sims = 5,
                                 r.samples = c.2[, 1], prog.count = FALSE))
  # Resource abundance the wrong length
  expect_error(generate_null_net(c.1, rbind(r.1, r.1), sims = 5,
                                 prog.count = FALSE))
  # C.samples too short
  expect_error(generate_null_net(c.2[, 2:12], r.2[, 2:11], sims = 5,
                                 c.samples = c.2[-1, 1], r.samples = r.2[, 1],
                                 prog.count = FALSE))
  # Abundance weights not 0-1
  expect_error(generate_null_net(c.1, r.1, sims = 5, r.weights = fbls.1.err2,
                                 prog.count = FALSE))
  # Non-integer values present in nominal consumer (i.e. data.type = "names")
  expect_error(generate_null_net(c.1.err1, r.1, sims = 5, prog.count = FALSE))
  # Values >1 present in nominal consumer (i.e. data.type = "names")
  expect_error(generate_null_net(c.1.c, r.1, sims = 5, prog.count = FALSE))
  # Non-integer values present in count data (i.e. data.type = "counts")
  expect_error(generate_null_net(c.1.q, r.1, sims = 5, data.type = "counts",
                                 prog.count = FALSE))
})


# Check that a warning is generated when an interaction is recorded with a
#   resource that has an abundance of zero in 'resources' i.e.  cannot be
#   selected by a consumer in the null model
test_that("Zero abundance warning", {
  expect_warning(generate_null_net(c.1, r.1.warn1, sims = 5, prog.count = FALSE))
})


# Check the marginal totals: i) that column sums are highly correlated with the
#  resource abundances (r > 0.9) and ii) that the resource sums match between the
#  observed and simulated data (number of resources, counts or quantities).
test_that("Marginal totals are correct", {
  skip_on_cran()
  n.1 <- generate_null_net(c.1, r.1, sims = 100, prog.count = FALSE)
  n.2 <- generate_null_net(c.1.m, r.1, sims = 100, prog.count = FALSE)
  n.3 <- generate_null_net(c.1.c, r.1, sims = 100, data.type = "counts",
                           prog.count = FALSE)
  n.4 <- generate_null_net(c.1.q, r.1, sims = 100, data.type = "quantities",
                           prog.count = FALSE)
  expect_true(cor(colSums(n.1$rand.data[, 3:ncol(n.1$rand.data)]), t(r.1)) >.9)
  expect_true(cor(colSums(n.2$rand.data[, 3:ncol(n.2$rand.data)]), t(r.1)) >.9)
  expect_true(cor(colSums(n.3$rand.data[, 3:ncol(n.3$rand.data)]), t(r.1)) >.9)
  expect_true(cor(colSums(n.4$rand.data[, 3:ncol(n.4$rand.data)]), t(r.1)) >.9)
  expect_equal(sum(rowSums(n.1$rand.data[n.1$rand.data$Consumer == "A",
                   -c(1, 2)]) == sum(c.1[c.1$Consumer == "A", -1])), 100)
  expect_equal(sum(rowSums(n.1$rand.data[n.1$rand.data$Consumer == "B",
                   -c(1, 2)]) == sum(c.1[c.1$Consumer == "B", -1])), 100)
  expect_equal(sum(rowSums(n.2$rand.data[n.2$rand.data$Consumer == "A",
                   -c(1, 2)]) == sum(c.1.m[c.1.m$Consumer == "A", -1])), 100)
  expect_equal(sum(rowSums(n.2$rand.data[n.2$rand.data$Consumer == "B",
                   -c(1, 2)]) == sum(c.1.m[c.1.m$Consumer == "B", -1])), 100)
  expect_equal(sum(rowSums(n.3$rand.data[n.3$rand.data$Consumer == "A",
                   -c(1, 2)]) == sum(c.1.c[c.1.c$Consumer == "A", -1])), 100)
  expect_equal(sum(rowSums(n.3$rand.data[n.3$rand.data$Consumer == "B",
                   -c(1, 2)]) == sum(c.1.c[c.1.c$Consumer == "B", -1])), 100)
  expect_equal(sum(abs(rowSums(n.4$rand.data[n.4$rand.data$Consumer == "A",
               -c(1, 2)]) - sum(c.1.q[c.1.q$Consumer == "A", -1])) < .001), 100)
  expect_equal(sum(abs(rowSums(n.4$rand.data[n.4$rand.data$Consumer == "B",
               -c(1, 2)]) - sum(c.1.q[c.1.q$Consumer == "B", -1])) < .001), 100)
})


# Check that resources assigned a weight of zero (i.e. forbidden links) are not
#  selected by the model
test_that("Resource weights work correctly", {
  skip_on_cran()
  n.5 <- generate_null_net(c.1, r.1, sims = 100, r.weights = fbls.1,
                           prog.count = FALSE)
  expect_identical(colSums(n.5$rand.data[, -(1:2)]) == 0,
                     colSums(fbls.1[, -1]) == 0)
})
