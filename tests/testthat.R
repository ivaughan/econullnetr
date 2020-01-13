Sys.setenv("R_TESTS" = "")

library(testthat)
library(econullnetr)

test_check("econullnetr")
