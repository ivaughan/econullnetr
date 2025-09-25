#' Plot a bipartite network, colour coding individual links according to
#'   whether they are stronger or weaker than expected under the null model
#'
#' Acts as a wrapper for the \code{bipartite} package's
#'   \code{\link[bipartite]{plotweb}} function (Dormann \emph{et al}., 2008),
#'   colour coding the links in the familiar bipartite plots according to
#'   whether they are stronger, weaker or consistent with the null model.
#'   Following revisions to \code{plotweb} in \code{bipartite} v2.22, this
#'   currently uses the older version of this function, now called
#'   \code{\link[bipartite]{plotweb_deprecated}}.
#'
#' @param nullnet An object of class "nullnet" from \code{generate_null_net}
#' @param signif.level An optional value specifying the threshold used for
#'   testing for 'significant' deviations from the null model.  Defaults to 0.95
#' @param edge.cols An optional character vector of length three specifying
#'   the colours for links in the bipartite plot: they should represent
#'   interactions that are weaker than expected, consistent with the null model
#'   and stronger than expected in that order.  The default is a colourblind
#'   friendly blue, white and red scheme, using colorbrewer's Red-Blue
#'   colour scheme (Brewer 2017).
#' @param ... Other arguments to be supplied to \code{bipartite}'s
#'   \code{plotweb_deprecated} function.
#'
#' @details Extensive options can be passed to
#'   \code{\link[bipartite]{plotweb_deprecated}} to customise the network plot
#'   beyond the colour coding of the links. See the appropriate help file in the
#'   bipartite package for details.
#'
#' @return No return value, called for side effects.
#'
#' @seealso \code{\link{generate_null_net}}, \code{\link{bipartite_stats}},
#'   \code{\link[bipartite]{plotweb_deprecated}}
#'
#' @references Brewer, C.A. (2017) \url{https://colorbrewer2.org/}
#'
#'   Dormann, C.F., Gruber B. & Frund, J. (2008). Introducing the bipartite
#'   package: analysing ecological networks. \emph{R news}, \strong{8}, 8-11.
#'
#'   Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E., Woodward, G. &
#'   Symondson, W.O.C. (2018) econullnetr: an R package using null models to
#'   analyse the structure of ecological networks and identify resource
#'   selection. \emph{Methods in Ecology and Evolution}, \strong{9}, 728--733.
#'
#' @examples
#'
#' # Run the null model
#' set.seed(1234)
#' sil.null <- generate_null_net(Silene[, 2:7], Silene.plants[, 2:6], sims = 10,
#'                               c.samples = Silene[, 1],
#'                               r.samples = Silene.plants[, 1])
#'
#' # Basic plot
#' plot_bipartite(sil.null)
#'
#' # With alternative colour scheme and nodes width in the lower level proportional
#' #  to mean floral abundance
#' mean.abunds <- colMeans(Silene.plants[, 2:6])
#' plot_bipartite(sil.null, signif.level = 0.95, edge.cols = c("#67a9cf",
#'                "#F7F7F7", "#ef8a62"), low.abun = mean.abunds,
#'                abuns.type = "independent", low.abun.col = "black",
#'                high.abun.col = "black", high.lablength = 0, low.lablength = 0)
#'
#' @export



plot_bipartite <- function(nullnet, signif.level = 0.95,
                           edge.cols = c("#67A9CF", "#F7F7F7", "#EF8A62"), ...) {
  # --------------------------------------
  # Return an error if a object from 'generate_null_net' is not supplied
  if(!inherits(nullnet, "nullnet")) stop(
    "plot_bipartite requires a nullnet object")
  # --------------------------------------
  # Set significance level
  if (signif.level <= 0 || signif.level >= 1) {
    stop("Invalid percentile value specified")}
  p <- (1 - signif.level) / 2
  # --------------------------------------

  # --------------------------------------
  # Set up observed data into a bipartite format
  obs.web <- nullnet$obs.interactions
  rownames(obs.web) <- obs.web[, 1]
  obs.web <- obs.web[, -1]
  obs.web <- t(obs.web)
  # --------------------------------------

  # --------------------------------------
  # Set up null data into a bipartite format
  null.mean <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                                list(nullnet$rand.data$Consumer), mean)
  rownames(null.mean) <- null.mean[,1 ]; null.mean <- null.mean[, -1]
  null.mean <- t(null.mean)
  null.upp <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                               list(nullnet$rand.data$Consumer), stats::quantile,
                               probs = 1 - p)
  rownames(null.upp) <- null.upp[,1 ]; null.upp<- null.upp[, -1]
  null.upp<- t(null.upp)
  null.low <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                               list(nullnet$rand.data$Consumer), stats::quantile,
                               probs = p)
  rownames(null.low) <- null.low[,1 ]; null.low <- null.low[, -1]
  null.low <- t(null.low)

  # Set the consumer order to match the observed web
  null.mean <- null.mean[match(rownames(obs.web), rownames(null.mean)), ]
  null.upp <- null.upp[match(rownames(obs.web), rownames(null.upp)), ]
  null.low <- null.low[match(rownames(obs.web), rownames(null.low)), ]
  # --------------------------------------

  # --------------------------------------
  # Prepare vector for colour coding individual links based upon null model results
  n.consumers <- ncol(obs.web)
  n.resources <- nrow(obs.web)
  select.vector <- matrix(rep(edge.cols[2],length = n.consumers * n.resources),
                          ncol = n.consumers, nrow = n.resources)
  select.vector <- ifelse(obs.web > null.upp, edge.cols[3],
                          ifelse(obs.web < null.low, edge.cols[1], select.vector))
  select.vector <- as.vector(t(select.vector))
  # --------------------------------------

  # --------------------------------------
  # Generate bipartite web plot
  bipartite::plotweb_deprecated(obs.web, method = "normal", empty = FALSE,
                     col.interaction = select.vector, ...)
}

