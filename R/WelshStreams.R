#' Part of the food web from upland streams in south Wales, UK
#'
#' Part of a food web from upland streams in south Wales, UK, comprising 85
#'   individuals of two macroinvertebrate predator species (the caddisfly
#'   \emph{Rhyacophila dorsalis} and the stonefly \emph{ Dinocras cephalotes}).
#'   Data were collected in Dec 2013 from six streams spread across an
#'   agricultural intensity gradient as part of a wider study (Pearson
#'   \emph{et al}. 2018). The data comprise presence or absence of predation
#'   by each predator on 16 potential prey taxa determined using next
#'   generation sequencing of predator gut contents.
#'   There are three accompanying data sets:
#'   \enumerate{
#'     \item \code{\link{WelshStreams.prey}}, which gives the mean abundance of each
#'       prey taxon in each of the six streams (counts of individuals from 3--min
#'       kick samples)
#'     \item \code{\link{WelshStreams.fl}} which specifies forbidden links (one
#'       forbidden link for each predator species, ruling out cannibalism)
#'     \item \code{\link{WelshStreams.order}} which ranks the 16 prey taxa in
#'       taxonomic order according to the Centre for Ecology and Hydrology's
#'       Coded Macroinvertebrate List. This is used for plotting the results.
#'   }
#'
#' @format A data frame with 85 rows and 18 variables. Each row represents the gut
#'   contents of an individual predator. The first column \code{Stream} indicates
#'   which of the six streams each predator was collected from, whilst the second
#'   column indicates which genus the predator belonged to (\emph{Dinocras} or
#'   \emph{Rhyacophila}). The remaining 16 columns represent the potential prey
#'   taxa, which were either predated (1) or not (0) by each individual predator.
#'
#' @source Pearson, C.E., Symondson, W.O.C., Clare, E.L., Ormerod, S.J.,
#'  Iparraguirre Bolanos, E. & Vaughan, I.P. (2018) The effects of pastoral
#'  intensification on the feeding interactions of generalist predators in
#'  streams. \emph{Molecular Ecology}, \strong{27}, 590-602.

"WelshStreams"
