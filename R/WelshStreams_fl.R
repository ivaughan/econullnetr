#' 'Forbidden' links to accompany the upland streams food web
#'
#' A table specifying trophic links that are allowed when modelling part of the
#'   food web from upland Welsh stream containing the two macroinvetebrate
#'   predators \emph{Rhyacophila dorsalis} and \emph{Dinocras cephalotes}
#'   (see \code{\link{WelshStreams}}).  There is one forbidden link for each
#'   predator species, preventing cannibalism from being included in the null
#'   model: this is because the method used to screen predator gut contents for
#'   prey (next generation sequencing) cannot distinguish between predator and
#'   prey of the same species.  Both predators are generalists, so all other
#'   trophic links are permitted.
#'
#' @format A data frame with 2 rows and 17 columns, each row representing one of the
#'   two predator genera. The first column (\code{Predator}) indicates the predator
#'   genus, whilst columns 2--17 indicate which of the 16 potential prey taxa can be
#'     consumed (1) or are forbidden (0).
#'
#' @source Pearson, C.E. (2015) \emph{Effects of agricultural intensification
#'   on the ecology of upland stream invertebrate communities}. Unpublished
#'   PhD thesis, Cardiff University.

"WelshStreams.fl"
