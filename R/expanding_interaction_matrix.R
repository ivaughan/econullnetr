#' Expand a summarised interaction matrix
#'
#' A simple function for converting interaction matrices that are summarised
#'   at (typically) species-level to individual-level matrices, ready for
#'   use with \code{generate_null_net}. This is only applicable to the
#'   special (but common) case where one individual = one interaction
#'   (e.g. many pollination networks, ant-seed networks).
#' Data can be stored either with consumers as columns and resources as
#'   rows or vice versa. Taxon names for each row in the matrix could either
#'   be stored as the row names of the matrix or data frame (as used, for
#'   example, by the \code{bipartite} package), or as a column containing the
#'   names in a data frame.

#' @param X A matrix or data frame representing the interaction matrix. This
#'   should only include the interaction data (i.e. counts of different
#'   interactions) and no additional columns for taxon names, covariates, etc
#' @param r.names An optional object of identical length to the number of
#'   rows in \code{X} listing the taxon names. In many situations these may
#'   be the row names of \code{X} (the default). Alternatively
#'   \code{r.names} can be use to specify a column in a data frame
#'   containing the names or a separate vector.
#' @param MARGIN Similar to \code{apply}, an integer value indicating
#'   whether the data are arranged in rows or columns. \code{MARGIN = 1}
#'   (the default) indicates that each column relates to one consumer taxon
#'   (the format typically used for bipartite networks), whilst
#'   \code{MARGIN = 2} indicates that each row is one consumer taxon, with
#'   column names being the resources.
#'
#' @return A data frame where each row represents the interaction observed
#'   between an individual consumer and one resource species. The first
#'   column is named \code{Consumer} and records which taxon each indidual
#'   belongs to. The remaining columns represent the resources: one column for each
#'   taxon.
#'
#' @examples
#' # Toy example representing a typical bipartite format.
#'   bp.inter <- matrix(c(1, 2, 2, 0, 5, 3, 3, 0, 2), nrow = 3, byrow = FALSE,
#'                      dimnames = list(c("A", "B", "C"),
#'                                      c("sp1", "sp2", "sp3")))
#'  bp.inter
#'  expand_matrix(bp.inter)
#'
#' # Use a simplified version of the Silene data set, pooling data
#' # across the 11 visits.
#'   int.summ <- aggregate(Silene[, 3:7], by = list(Silene$Insect), sum)
#'   colnames(int.summ)[1] <- "taxon"
#'   expand_matrix(int.summ[, -1], r.names = int.summ$taxon, MARGIN = 2)
#'
#' @export


expand_matrix <- function(X, r.names = rownames(X), MARGIN = 1) {

  # --------------------------------------
  # Return an error if MARGIN value <> 1 or 2
  if(!(MARGIN == 1 | MARGIN == 2)) stop(
    "MARGIN must be 1 or 2")
  # --------------------------------------

  # --------------------------------------
  # Return an error if duplicate taxon names are present in rows or columns
  if(length(unique(colnames(X))) < ncol(X) |
     length(unique(r.names)) < nrow(X)) stop(
    "Duplicate row or column names present")
  # --------------------------------------

  # --------------------------------------
  # Return an error if non-integer values are present in the matrix
  if(sum(abs(X - round(X, 0))) > 0) stop(
       "Non-integer values present")
  # --------------------------------------

  # Create data frame where rows = consumers and columns = resources
  ifelse(MARGIN == 1, {
    imat <- data.frame(X, row.names = r.names)
    imat <- t(imat)
    imat <- data.frame(Consumer = rownames(imat), imat, row.names = NULL)
  }, {
    imat <- data.frame(Consumer = r.names, X)
  })


  # Convert to long format, replicate rows by the number
  imat <- reshape2::melt(imat, id.vars = "Consumer")
  imat <- imat[rep(1:nrow(imat), imat[, 3]), -3]
  imat$ID.code <- seq(1, nrow(imat))
  imat$count <- 1
  #imat <- reshape2::dcast(imat, Consumer + ID.code ~ variable,
  #                        fun.aggregate = length, fill = 0)
  imat <- reshape2::dcast(imat, Consumer + ID.code ~ variable,
                          value.var = "count",
                          fun.aggregate = length, fill = 0)
  imat$ID.code <- NULL


  return(imat)
}

