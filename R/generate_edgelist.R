#' Export null modelling results
#'
#' Exports the observed network alongside the mean interaction strengths
#'   calculated from the null model and the significance test results in a
#'   standard format that can be imported into other network analysis packages
#'   e.g. \code{igraph} (Csardi & Nepusz 2006) or \code{cheddar} (Hudson
#'   \emph{et al}. 2013).  This provides access to a wide range of plotting and
#'   analysis tools, especially for non-bipartite networks.
#'
#' @param nullnet An object of class nullnet from \code{generate_null_net}
#' @param signif.level An optional value specifying the threshold used for
#'   testing for 'significant' deviations from the null model.  Defaults to 0.95
#' @param export.null An optional logical value specifying whether export should
#'   be limited to those interactions that were present in the observed network
#'   or should include any additional interactions present across iterations
#'   of the null network.  Depending upon the data and any forbidden links
#'   specified, additional interactions may be present in the modelled networks.
#'   Defaults to FALSE (only observed interactions are exported).
#' @param edge.cols An optional character vector of length three specifying
#'   potential colours for network links when used with a suitable plotting
#'   function: in sequence, these should represent i) interactions that are
#'   weaker than expected, ii) consistent with the null model and iii) stronger
#'   than expected. The default is a colourblind friendly blue, white and red
#'   scheme, using Colorbrewer's Red-Blue colour scheme (Brewer 2017).
#'
#' @return A data frame where each row represents the interaction observed
#'   between a pair of consumer and resource species, and with the following
#'   column headings:
#'   \describe{
#'     \item{\code{Consumer}}{Name of the consumer species (node)}
#'     \item{\code{Resource}}{Name of the resource species (node)}
#'     \item{\code{Observed}}{Strength of the observed interaction}
#'     \item{\code{Null}}{Mean strength of the interaction across the iterations
#'       of the null model}
#'     \item{\code{SES}}{The standardised effect size for the interaction}
#'     \item{\code{Test}}{Whether the observed interaction is significantly
#'       \strong{stronger} than expected under the null model, \strong{weaker}
#'       or consistent with the null model \strong{ns}}
#'   }
#'
#' @seealso \code{\link{generate_null_net}}
#'
#' @references Brewer, C.A. (2017) \url{http://www.ColorBrewer.org}
#'
#'   Csardi, G. & Nepusz, T. (2006) The igraph software package for complex
#'   network research. \emph{InterJournal, Complex Systems}, \strong{1695}.
#'
#'   Hudson, L.N., Emerson, R., Jenkins, G.B., Layer, K., Ledger, M.E., Pichler,
#'   D.E., Thompson, M.S.A., O'Gorman, E.J., Woodward, G & Reuman, D.C. (2013)
#'   Cheddar: analysis and visualisation of ecological communities in R.
#'   \emph{Methods in Ecology and Evolution}, \strong{4}, 99--104.
#'
#'   Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E., Woodward, G. &
#'   Symondson, W.O.C. (2018) econullnetr: an R package using null models to
#'   analyse the structure of ecological networks and identify resource
#'   selection. \emph{Methods in Ecology and Evolution}, \strong{9}, 728--733.
#'
#' @examples
#' set.seed(1234)
#' bs.null <- generate_null_net(Broadstone, Broadstone.prey, data.type = "counts",
#'                              sims = 10, r.weights = Broadstone.fl)
#' BS.export <- generate_edgelist(bs.null, signif.level = 0.95)
#'
#' # Plot network with functions from the cheddar package
#' library(cheddar)
#' BS.comm <- list(title = "Broadstone, August", M.units = "mg",
#'                 N.units = "counts")
#'
#' # Change to lower case to match cheddar convention, then create an object
#' #  of class 'community'
#' colnames(BS.export)[1:2] <- c("consumer", "resource")
#' BS <- Community(nodes = Broadstone.nodes, properties = BS.comm,
#'                 trophic.links = BS.export)
#'
#' PlotWebByLevel(BS, link.colour.by = "Test", link.colour.spec = c(Stronger =
#'                "#d7191c", ns = "#cccccc", Weaker = "#2c7bb6"),
#'                link.lwd = log(TLPS(BS)$Observed), pch = 16, cex = 3,
#'                col = "black", highlight.nodes = NULL,
#'                show.nodes.as = "both", label.cex = 1, label.colour = "white")
#' legend("topright", legend = c("Stronger", "ns", "Weaker"), lty = 1, lwd = 2,
#'        col = c("#d7191c", "#cccccc", "#2c7bb6"))
#'
#' @export


generate_edgelist <- function(nullnet, signif.level = 0.95, export.null = FALSE,
                              edge.cols = c("#67A9CF", "#F7F7F7", "#EF8A62")) {
  # --------------------------------------
  # Return an error if a object from 'generate_null_net' is not supplied
  if(class(nullnet) != "nullnet") stop(
    "generate_edgelist requires a nullnet object")
  # --------------------------------------
  # --------------------------------------
  # Set significance level
  if (signif.level <= 0 || signif.level >= 1) {
    stop("Invalid percentile value specified")}
  p <- (1 - signif.level) / 2
  # --------------------------------------
  # --------------------------------------
  # Summarise modelled interactions
  null.mean <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                                list(nullnet$rand.data$Consumer), mean)
  null.upp <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                               list(nullnet$rand.data$Consumer), stats::quantile,
                               probs = 1 - p)
  null.low <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                              list(nullnet$rand.data$Consumer), stats::quantile,
                              probs = p)
  sd.inters <- stats::aggregate(nullnet$rand.data[, 3:ncol(nullnet$rand.data)],
                         list(nullnet$rand.data$Consumer), stats::sd)
  # --------------------------------------

  # --------------------------------------
  # Convert to long format using 'melt'
  obs.inters <- reshape2::melt(nullnet$obs.interactions, id.vars = "Consumer")
  colnames(obs.inters)[2] <- "Resource"
  null.mean <- reshape2::melt(null.mean, id.vars = "Group.1")
  colnames(null.mean)[c(1, 2)] <- c("Consumer", "Resource")
  null.low <- reshape2::melt(null.low, id.vars = "Group.1")
  colnames(null.low)[c(1, 2)] <- c("Consumer", "Resource")
  null.upp <- reshape2::melt(null.upp, id.vars = "Group.1")
  colnames(null.upp)[c(1, 2)] <- c("Consumer", "Resource")
  sd.inters <- reshape2::melt(sd.inters, id.vars = "Group.1")
  colnames(sd.inters)[c(1, 2)] <- c("Consumer", "Resource")
  # --------------------------------------

  # --------------------------------------
  # Merge the observed interaction strengths with those from the null model,
  #   the upper and lower confidence limits and standard deviation
  output <- merge(obs.inters, null.mean, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[c(3,4)]<-c("Observed", "Null")
  output <- merge(output, null.low, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[5] <- paste('Lower.', signif.level * 100, '.CL', sep = "")
  output <- merge(output, null.upp, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[6] <- paste('Upper.', signif.level * 100, '.CL', sep = "")
  output <- merge(output, sd.inters, by.x = c("Consumer", "Resource"),
                  by.y = c("Consumer", "Resource"))
  colnames(output)[7] <- "sd"
  # --------------------------------------

  # --------------------------------------
  # Test significance, comparing observed link strength to the CLs
  output$Test <- as.character(rep("ns"))
  output$edge.col <- rep(edge.cols[2])
  for (i in 1:nrow(output)) { # Uses partial matching
    if (output$Observed[i] > output$Upper[i])
       {output$Test[i] <- "Stronger"; output$edge.col[i] <- edge.cols[3] }
    if (output$Observed[i] < output$Lower[i])
       {output$Test[i] <- "Weaker"; output$edge.col[i] <- edge.cols[1] }
  }
  # --------------------------------------

  # --------------------------------------
  # Calculate the standard effect sizes for observed v. null interactions
  output$SES <- with(output, (Observed - Null) / sd)
  output$SES[!is.finite(output$SES)] <- NA
  # --------------------------------------

  # --------------------------------------
  # Trim output table for concise export format
  output <- output[, c(1:4, 10, 8, 9)]
  if(export.null == FALSE) output <- output[output$Observed != 0, ]
  if(export.null == TRUE) output <- output[output$Null != 0, ]
  # --------------------------------------

return(output)
}


