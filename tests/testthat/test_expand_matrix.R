library(econullnetr)
context("expand_matrix")

# Prepare data sets to test the function
# Correct expanded version of the interaction data
test.mat.1 <- utils::read.csv(system.file("testdata", "expanded_matrix.csv",
                                          package = "econullnetr"))
test.mat.1 <- test.mat.1[order(test.mat.1$Consumer, test.mat.1$A,
                               test.mat.1$B, test.mat.1$C, test.mat.1$D), ]
rownames(test.mat.1) <- NULL

# A. Data sets with consumers in columns (i.e. MARGIN = 1)
# Bipartite format
d1 <- matrix(c(3, 5, 2, 0, 3, 1, 10, 3, 12, 0, 0, 0, 5, 9, 2, 10, 1, 3, 0,
               0), nrow = 5, byrow = FALSE,
             dimnames = list(c("A", "B", "C", "D", "E"),
                             c("sp1", "sp2", "sp3", "sp4")))

# Data frame equivalent of d1
d2 <- data.frame(sp1 = c(3, 5, 2, 0, 3), sp2 = c(1, 10, 3, 12, 0),
                 sp3 = c(0, 0, 5, 9, 2), sp4 = c(10, 1, 3, 0, 0))
rownames(d2) <- c("A", "B", "C", "D", "E")

# Data frame with resource column
d3 <- data.frame(resource = rownames(d1), d1, row.names = NULL)

# Check read.csv and read_csv (readr/tibble) compatibility
d4 <- utils::read.csv(system.file("testdata", "expand_matrix_test.csv",
                                  package = "econullnetr"))
d5 <- readr::read_csv(system.file("testdata", "expand_matrix_test.csv",
                                  package = "econullnetr"))


# B. Data sets with resources in columns (i.e. MARGIN = 2)
d6 <- matrix(c(3, 5, 2, 0, 3, 1, 10, 3, 12, 0, 0, 0, 5, 9, 2, 10, 1, 3, 0,
               0), nrow = 4, byrow = FALSE,
             dimnames = list(c("sp1", "sp2", "sp3", "sp4"),
                             c("A", "B", "C", "D", "E")))

# Data frame equivalent
d7 <- data.frame(A = c(3, 5, 2, 0), B = c(3, 1, 10, 3), C = c(12, 0, 0, 0),
                 D = c(5, 9, 2, 10), E = c(1, 3, 0, 0))
rownames(d7) <- c("sp1", "sp2", "sp3", "sp4")

# Data frame with consumer column
d8 <- data.frame(consumer = rownames(d6), d6, row.names = NULL)


# Data sets with consumers and/or resources not in ascending order
d9 <- matrix(c(0, 1, 3, 0, 10, 0, 10, 3, 12, 1, 2, 0, 5, 9, 0, 3, 5, 2, 0,
               3), nrow = 5, byrow = FALSE,
             dimnames = list(c("E", "B", "C", "D", "A"),
                             c("sp4", "sp2", "sp3", "sp1")))

d10 <- matrix(c(0, 0, 2, 3, 1, 10, 0, 5, 3, 3, 5, 2, 0, 12, 9, 0, 10, 1, 0,
                3), nrow = 4, byrow = FALSE,
              dimnames = list(c("sp4", "sp2", "sp3", "sp1"),
                              c("E", "B", "C", "D", "A")))

# Data sets with duplicate resource or consumer names
d11 <- d1
rownames(d11)[2] <- "A"
d12 <- d1
colnames(d12)[2] <- "sp1"
d13 <- d6
rownames(d13)[2] <- "sp1"
d14 <- d6
colnames(d14)[2] <- "A"

# Data set with non-integer values
d15 <- matrix(c(3, 5, 2.1, 0, 3, 1, 10, 3, 12, 0, 0, 0, 4.9, 9, 2, 10, 1, 3,
                0, 0), nrow = 5, byrow = FALSE,
             dimnames = list(c("A", "B", "C", "D", "E"),
                             c("sp1", "sp2", "sp3", "sp4")))


# Expand and sort the matrices
d1.ex <- expand_matrix(d1)
d1.ex <- data.frame(Consumer = d1.ex$Consumer,
                    sapply(d1.ex[, -1], as.integer))
d1.ex <- d1.ex[order(d1.ex$Consumer, d1.ex$A, d1.ex$B, d1.ex$C, d1.ex$D,
                     d1.ex$E), ]
rownames(d1.ex) <- NULL

d2.ex <- expand_matrix(d2)
d2.ex <- data.frame(Consumer = d2.ex$Consumer,
                    sapply(d2.ex[, -1], as.integer))
d2.ex <- d2.ex[order(d2.ex$Consumer, d2.ex$A, d2.ex$B, d2.ex$C, d2.ex$D,
                     d1.ex$E), ]
rownames(d2.ex) <- NULL

d3.ex <- expand_matrix(d3[, -1], r.names = d3$resource)
d3.ex <- data.frame(Consumer = d3.ex$Consumer,
                    sapply(d3.ex[, -1], as.integer))
d3.ex <- d3.ex[order(d3.ex$Consumer, d3.ex$A, d3.ex$B, d3.ex$C, d3.ex$D,
                     d1.ex$E), ]
rownames(d3.ex) <- NULL

d4.ex <- expand_matrix(d4[, -1], r.names = d4$resource)
d4.ex <- data.frame(Consumer = d4.ex$Consumer,
                    sapply(d4.ex[, -1], as.integer))
d4.ex <- d4.ex[order(d4.ex$Consumer, d4.ex$A, d4.ex$B, d4.ex$C, d4.ex$D,
                     d1.ex$D), ]
rownames(d4.ex) <- NULL

d5.ex <- expand_matrix(d5[, -1], r.names = d5$resource)
d5.ex <- data.frame(Consumer = d5.ex$Consumer,
                    sapply(d5.ex[, -1], as.integer))
d5.ex <- d5.ex[order(d5.ex$Consumer, d5.ex$A, d5.ex$B, d5.ex$C, d5.ex$D,
                     d1.ex$D), ]
rownames(d5.ex) <- NULL

d6.ex <- expand_matrix(d6, MARGIN = 2)
d6.ex <- data.frame(Consumer = d2.ex$Consumer,
                    sapply(d2.ex[, -1], as.integer))
d6.ex <- d6.ex[order(d6.ex$Consumer, d6.ex$A, d6.ex$B, d6.ex$C, d6.ex$D,
                     d1.ex$D), ]
rownames(d6.ex) <- NULL

d7.ex <- expand_matrix(d7, MARGIN = 2)
d7.ex <- data.frame(Consumer = d2.ex$Consumer,
                    sapply(d2.ex[, -1], as.integer))
d7.ex <- d7.ex[order(d7.ex$Consumer, d7.ex$A, d7.ex$B, d7.ex$C, d7.ex$D,
                     d1.ex$D), ]
rownames(d7.ex) <- NULL

d8.ex <- expand_matrix(d8[, -1], r.names = d8$consumer, MARGIN = 2)
d8.ex <- data.frame(Consumer = d2.ex$Consumer,
                    sapply(d2.ex[, -1], as.integer))
d8.ex <- d8.ex[order(d8.ex$Consumer, d8.ex$A, d8.ex$B, d8.ex$C, d8.ex$D,
                     d1.ex$D), ]
rownames(d8.ex) <- NULL

d9.ex <- expand_matrix(d9)
d9.ex <- data.frame(Consumer = d9.ex$Consumer,
                    sapply(d9.ex[, -1],
                           as.integer)[, order(colnames(d9.ex[-1]))])
d9.ex <- d9.ex[order(d9.ex$Consumer, d9.ex$A, d9.ex$B, d9.ex$C, d9.ex$D,
                     d1.ex$E), ]
rownames(d9.ex) <- NULL


d10.ex <- expand_matrix(d10, MARGIN = 2)
d10.ex <- data.frame(Consumer = d10.ex$Consumer,
                     sapply(d10.ex[, -1],
                            as.integer)[, order(colnames(d10.ex[-1]))])
d10.ex <- d10.ex[order(d10.ex$Consumer, d10.ex$A, d10.ex$B, d10.ex$C,
                       d10.ex$D, d1.ex$E), ]
rownames(d10.ex) <- NULL

# ------------------------
# Run the tests
test_that("Check error handling: MARGIN, duplicate names and nonintegers",{
  expect_error(expand_matrix(d1, MARGIN = 0.5))
  expect_error(expand_matrix(d1, MARGIN = 1.5))
  expect_error(expand_matrix(d6, MARGIN = 2.5))
  expect_error(expand_matrix(d11))
  expect_error(expand_matrix(d12))
  expect_error(expand_matrix(d13, MARGIN = 2))
  expect_error(expand_matrix(d14, MARGIN = 2))
  expect_error(expand_matrix(d15))
})


test_that("Check that results are the same specifying rownames or not", {
  expect_identical(expand_matrix(d1, rownames(d1)), expand_matrix(d1))
  expect_identical(expand_matrix(d6, rownames(d6), MARGIN = 2),
                   expand_matrix(d6, MARGIN = 2))
})


test_that("Expanded matrices match test data set",{
  expect_identical(d1.ex, test.mat.1)
  expect_identical(d2.ex, test.mat.1)
  expect_identical(d3.ex, test.mat.1)
  expect_identical(d4.ex, test.mat.1)
  expect_identical(d5.ex, test.mat.1)
  expect_identical(d6.ex, test.mat.1)
  expect_identical(d7.ex, test.mat.1)
  expect_identical(d8.ex, test.mat.1)
  expect_identical(d9.ex, test.mat.1)
  expect_identical(d10.ex, test.mat.1)
})
