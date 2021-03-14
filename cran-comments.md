## Resubmission
This is a resubmission. 
* Replaced expect_equal_to_reference function with expect_known_value in unit tests testthat/test_generate_edgelist.R and testthat/test_test_interactions.R to correct error in CRAN package check results.
* Updated URLs in man/generate_edgelist.Rd, man/plot_bipartite.Rd, man/plot_preferences.Rd, R/generate_edgelist.R, R/plot_bipartite.R and R/plot_preferences.R.
* Updated doi format in DESCRIPTION.
* Added @return tag to plot_bipartite.R and plot_preferences.R, and \value to plot_bipartite.Rd and plot_preferences.Rd.
* User options re-set in plot_preferences.R and plot_preferences.Rd, and in inst/doc/econullnetr-intro.R.

## Test environments
* local Windows 10, R 4.0.3
* Ubuntu 16.04 on Travis CI, R 4.0.2 and R-devel.
* Mac OS X 10.13.6 on Travis CI, R 4.0.3.

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for this package.
