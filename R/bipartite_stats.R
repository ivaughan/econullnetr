#' Test for significant differences in a range of network metrics between the
#'   observed and null bipartite networks
#'
#' Acts as a wrapper for the \code{bipartite} package's
#'   \code{\link[bipartite]{networklevel}},
#'   \code{\link[bipartite]{grouplevel}} and
#'   \code{\link[bipartite]{specieslevel}} functions, allowing a wide range
#'   of measures to be calculated (Dormann \emph{et al}., 2008, 2009; Dorman
#'   2011).  These are calculated both for the observed network and across the
#'   iterations of the null model, allowing for simple tests of whether the
#'   observed values differ from those expected by chance.
#'
#' @param nullnet An object of class 'nullnet' from \code{generate_null_net}
#' @param signif.level An optional value specifying the threshold used for
#'   testing for 'significant' deviations from the null model.  Defaults to 0.95
#' @param index.type String specifying which function to call from the
#'   \code{bipartite} package: \code{specieslevel}, \code{grouplevel} or
#'   \code{networklevel}, according to the level at which the statistics are
#'   required.  For \code{specieslevel}, statistics are calculated for both
#'   'higher' and 'lower' levels i.e. \code{level = "both"} and cannot be
#'   changed: adding a \code{level} argument will produce an error:
#'   statistics from higher and lower levels are easily separated in the output
#'   from \code{bipartite_stats}.
#' @param indices Vector listing the bipartite network statistics to calculate.
#'   All indices are currently supported, with the exception of the dependence
#'   matrix: if \code{ALL} is specified, it will default to \code{ALLBUTD}.
#' @param ... Other arguments that may be supplied to \code{bipartite}'s
#'   \code{specieslevel}, \code{grouplevel} or \code{networklevel} functions.
#' @param prog.count A logical value specifying whether the progress count
#'   should be shown. Defaults to \code{TRUE}.

#'
#' @details Allows most of the network metrics in the \code{bipartite} package
#'   to be calculated for an observed bipartite network and compared to the
#'   distribution of those network metrics across the iterations of the null
#'   model.  This indicates whether the observed network differs from the
#'   structure of the network that could be expected if consumers simply used
#'   resources in proportion to their relative abundance.
#'
#'   The user sets the significance level (default = 95\% confidence limits),
#'   and the metrics selected are classified into those that are higher,
#'   lower or consistent with the null model at that significance level.
#'   Significance is determined by comparing the observed value of the statistic
#'   to the 1-alpha/2 percentiles from the frequency distribution, with
#'   'significant' values falling outside the confidence interval (Manly 2006).
#'
#' @return Returns one or more data frames according to the level at which the
#'   statistics are calculated (\code{specieslevel, grouplevel} or
#'   \code{networklevel}).  If \code{index.type = "networklevel"} or
#'   \code{index.type = "grouplevel"} a single data frame is returned, listing
#'   the chosen network statistics and with the following column headings:
#'   \describe{
#'     \item{\code{Observed}}{Value of the statistic for the observed network}
#'     \item{\code{Null}}{Mean value of the statistic across the iterations of
#'       the null model}
#'     \item{\code{Lower.CL}}{Lower confidence limit for the metric}
#'     \item{\code{Upper.CL}}{Upper confidence limit for the network metric}
#'     \item{\code{Test}}{Whether the value of the statistic with the observed
#'       network is significantly \strong{higher} than expected under the
#'       null model, \strong{lower} or consistent with the null model
#'       (\strong{ns})}
#'     \item{\code{SES}}{The standardised effect size for the difference between
#'       the observed network and the null model (see Gotelli & McCabe 2002
#'       for details)}
#'   }
#'
#'   If \code{index.type = "specieslevel"}, a list comprising two data frames for
#'     each statistic, representing the \code{higher} and \code{lower} levels in
#'     the network. Each data frame has the same format as for
#'     \code{networklevel} except that the rows are individual nodes (species)
#'     in the network. See examples for how to call the individual data frames.
#'
#' @seealso \code{\link{generate_null_net}}, \code{\link{plot_bipartite}},
#'   \code{\link[bipartite]{networklevel}}, \code{\link[bipartite]{grouplevel}},
#'   \code{\link[bipartite]{specieslevel}}
#'
#' @references Dormann, C.F., Gruber B. & Frund, J. (2008). Introducing the
#'   bipartite package: analysing ecological networks. \emph{R news},
#'   \strong{8}, 8--11.
#'
#'   Dormann, C.F., Frund, J., Bluthgen, N. & Gruber, B. (2009). Indices, graphs
#'   and null models: analyzing bipartite ecological networks. \emph{Open
#'   Ecology Journal} \strong{2}, 7--24.
#'
#'   Dormann, C.F. (2011) How to be a specialist? Quantifying specialisation
#'   in pollination networks. \emph{Network Biology}, \strong{1}, 1-20.
#'
#'   Gotelli, N.J. & McCabe, D.J. (2002) Species co-occurrence: a meta-analysis
#'   of J.M. Diamond's assembly rules model. \emph{Ecology}, \strong{83},
#'   2091--2096.
#'
#'   Manly, B.F.J. (2006) \emph{Randomization, Bootstrap and Monte Carlo Methods
#'   in Biology} (3rd edn). Chapman & Hall, Boca Raton.
#'
#'   Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E., Woodward, G. &
#'   Symondson, W.O.C. (2018) econullnetr: an R package using null models to
#'   analyse the structure of ecological networks and identify resource
#'   selection. \emph{Methods in Ecology and Evolution}, \strong{9}, 728--733.
#'
#' @examples
#' set.seed(1234)
#' sil.null <- generate_null_net(Silene[, 2:7], Silene.plants[, 2:6], sims = 10,
#'                               c.samples = Silene[, 1],
#'                               r.samples = Silene.plants[, 1])
#' # Network-level analysis
#' net.stats <- bipartite_stats(sil.null, index.type = "networklevel",
#'                              indices = c("linkage density",
#'                              "weighted connectance", "weighted nestedness",
#'                              "interaction evenness"), intereven = "sum")
#' net.stats
#'
#' # Group-level analysis
#' grp.stats <- bipartite_stats(sil.null, index.type = "grouplevel",
#'                              indices = c("generality",
#'                              "vulnerability", "partner diversity"),
#'                              logbase = 2)
#' grp.stats
#'
#' # Species-level statistics
#' spp.stats <- bipartite_stats(sil.null, index.type = "specieslevel",
#'                              indices = c("degree", "normalised degree",
#'                              "partner diversity"), logbase = exp(1))
#'
#' spp.stats # Show all data frames of results
#' spp.stats$normalised.degree # Select one statistic
#' spp.stats$normalised.degree$lower # Select one statistic at one level
#'
#' @export


bipartite_stats <- function(nullnet, signif.level = 0.95, index.type,
                            indices, prog.count = TRUE, ...) {
  # --------------------------------------
  # Return an error if a object from 'generate_null_net' is not supplied
  if(class(nullnet) != "nullnet") {
    stop("bipartite_stats requires a nullnet object")}
    # Return error if selected indices are not supported
  if("degree distribution" %in% indices) {
    stop("Degree distribution is not currently supported by bipartite_stats")}
  if("topology" %in% indices) {
    stop("Degree distribution (called by indices = 'topology') is not currently supported by bipartite_stats")}
    # --------------------------------------
  # Set significance level
  if (signif.level <= 0 || signif.level >= 1) {
    stop("Invalid percentile value specified")}
  p <- (1 - signif.level) / 2
  # --------------------------------------
  # Convert observed data into a bipartite format
  obs.web <- nullnet$obs.interactions
  rownames(obs.web) <- obs.web[, 1]
  obs.web <- obs.web[, -1]
  obs.web <- t(obs.web)
  # --------------------------------------


  # ======================================
  # Calculate the selected indices
  # ======================================
  if (index.type == "networklevel") {
    # --------------------------------------
    if("ALL" %in% indices){
      obs.net.stats <- bipartite::networklevel(obs.web,
                                               index = "ALLBUTDD", ...)
      } else {
      obs.net.stats <- bipartite::networklevel(obs.web, index = indices, ...)
    }
    net.stats <- matrix(ncol = length(obs.net.stats), nrow = 0)
    colnames(net.stats) <- names(obs.net.stats)
    # --------------------------------------
    # --------------------------------------
    # Calculate expected values of the statistics from iterations of the null model
    for (i in 1:nullnet$n.iterations) {
      web1 <- eval(parse(text = paste(
                   'nullnet$rand.data[nullnet$rand.data$Iteration == "It.',
                   i, '", ]', sep = "")))
      rownames(web1) <- web1$Consumer
      web1 <- web1[, -c(1:2)]
      web1 <- t(web1)
      if("ALL" %in% indices){
        net.stats <- rbind(net.stats, bipartite::networklevel(web1,
                                      index = "ALLBUTDD", ...))
        } else {
        net.stats <- rbind(net.stats, bipartite::networklevel(web1, indices, ...))
      }
      if (prog.count == TRUE){Sys.sleep(0.02); print(i); utils::flush.console()}
    }
    # --------------------------------------
    # --------------------------------------
    # Calculate the confidence limits and standardised effect sizes
    net.level.mean <- apply(net.stats, 2, mean, na.rm = TRUE)
    net.level.up <- apply(net.stats, 2, stats::quantile, probs = 1 - p, na.rm = TRUE)
    net.level.low <- apply(net.stats, 2, stats::quantile, probs = p, na.rm = TRUE)
    net.level.sd <- apply(net.stats, 2, stats::sd, na.rm = TRUE)
    net.level.ses <- (obs.net.stats - net.level.mean) / net.level.sd
    # --------------------------------------
    # --------------------------------------
    # Format the output table and add test results
    net.level.results <- data.frame(obs.net.stats, net.level.mean, net.level.low,
                                    net.level.up, rep("ns", length(net.level.low)),
                                    net.level.ses)
    colnames(net.level.results) <- c("Observed", "Null", "Lower.CL", "Upper.CL",
                                     "Test", "SES")
    net.level.results$Test <- as.character(net.level.results$Test)
    for (i in 1:nrow(net.level.results)) {
      if (is.na(net.level.results$Observed[i]))
         {net.level.results$Test[i] <- "NA"} else {
      if ((net.level.results$Observed[i] == net.level.results$Lower.CL[i]) &
         (net.level.results$Observed[i] == net.level.results$Upper.CL[i]))
         {net.level.results$Test[i] <- "NA"}
      if (net.level.results$Observed[i] > net.level.results$Upper.CL[i])
         {net.level.results$Test[i] <- "Higher"}
      if (net.level.results$Observed[i] < net.level.results$Lower.CL[i])
         {net.level.results$Test[i] <- "Lower"}
      }
    }
  }
  # --------------------------------------


  # ======================================
  if (index.type == "grouplevel") {
  # ======================================
    if("ALL" %in% indices){
      obs.net.stats <- bipartite::grouplevel(obs.web, index = "ALLBUTDD", ...)
      } else {
      obs.net.stats <- bipartite::grouplevel(obs.web, index = indices, ...)
    }
    net.stats <- matrix(ncol = length(obs.net.stats), nrow = 0)
    colnames(net.stats) <- names(obs.net.stats)
    # --------------------------------------
    # --------------------------------------
    for (i in 1:nullnet$n.iterations) {
      web1 <- eval(parse(text = paste(
                   'nullnet$rand.data[nullnet$rand.data$Iteration == "It.',
                   i, '", ]', sep = "")))
      rownames(web1) <- web1$Consumer
      web1 <- web1[, -c(1:2)]
      web1 <- t(web1)
      if("ALL" %in% indices){
        net.stats <- rbind(net.stats, bipartite::grouplevel(web1, "ALLBUTDD",
                                                            ...)) } else {
        net.stats <- rbind(net.stats, bipartite::grouplevel(web1, indices, ...))
      }
      if (prog.count == TRUE){Sys.sleep(0.02); print(i); utils::flush.console()}
    }
    # --------------------------------------
    # --------------------------------------
    net.level.mean <- apply(net.stats, 2, mean, na.rm = TRUE)
    net.level.up <- apply(net.stats, 2, stats::quantile, probs = 1 - p,
                          na.rm = TRUE)
    net.level.low <- apply(net.stats, 2, stats::quantile, probs = p,
                           na.rm = TRUE)
    net.level.sd <- apply(net.stats, 2, stats::sd, na.rm = TRUE)
    net.level.ses <- (obs.net.stats - net.level.mean) / net.level.sd
    # --------------------------------------
    # --------------------------------------
    grp.level.results <- data.frame(obs.net.stats, net.level.mean, net.level.low,
                                    net.level.up, rep("ns", length(net.level.low)),
                                    net.level.ses)
    colnames(grp.level.results) <- c("Observed", "Null", "Lower.CL", "Upper.CL",
                                     "Test", "SES")
    grp.level.results$Test <- as.character(grp.level.results$Test)
    for (i in 1:nrow(grp.level.results)) {
      if (is.na(grp.level.results$Observed[i]))
         {grp.level.results$Test[i] <- "NA"} else {
      if ((grp.level.results$Observed[i] == grp.level.results$Lower.CL[i]) &
         (grp.level.results$Observed[i] == grp.level.results$Upper.CL[i]))
         {grp.level.results$Test[i] <- "NA"}
      if (grp.level.results$Observed[i] > grp.level.results$Upper.CL[i])
         {grp.level.results$Test[i] <- "Higher"}
      if (grp.level.results$Observed[i] < grp.level.results$Lower.CL[i])
         {grp.level.results$Test[i] <- "Lower"}
      }
    }
  }
  # --------------------------------------


  # ======================================
  if (index.type == "specieslevel") {
  # Generates an array of matrices: one matrix per statistic
  # ======================================
    # Calculate observed values of the species-level statistics
    if("ALL" %in% indices){
      obs.net.stats <- bipartite::specieslevel(obs.web, index = "ALLBUTD",
                                               level = "both", ...)} else {
      obs.net.stats <- bipartite::specieslevel(obs.web, index = indices,
                                               level = "both", ...)
    }
    obs.high <- cbind(rownames(obs.net.stats$'higher level'),
                      obs.net.stats$'higher level')
    rownames(obs.high) <- NULL
    colnames(obs.high)[1] <- "species"
    obs.low <- cbind(rownames(obs.net.stats$'lower level'),
                     obs.net.stats$'lower level')
    rownames(obs.low) <- NULL
    colnames(obs.low)[1] <- "species"
    # --------------------------------------
    # --------------------------------------
    # Calculate expected values of the statistics from iterations of the null model
    for (i in 1:nullnet$n.iterations) {
      web1 <- eval(parse(text = paste(
                   'nullnet$rand.data[nullnet$rand.data$Iteration == "It.',
                   i, '", ]', sep = "")))
      rownames(web1) <- web1$Consumer
      web1 <- web1[, -c(1:2)]
      web1 <- t(web1)
      if("ALL" %in% indices){
        iteration.stats <- bipartite::specieslevel(web1, index = "ALLBUTD",
                                                 level = "both", ...)} else {
        iteration.stats <- bipartite::specieslevel(web1, index = indices,
                                                   level = "both", ...)
      }
      iteration.high <- cbind(rownames(iteration.stats$'higher level'),
                              iteration.stats$'higher level')
      rownames(iteration.high) <- NULL
      colnames(iteration.high)[1] <- "species"
      if (i == 1) { net.stats.high <- iteration.high } else {
        net.stats.high <- rbind(net.stats.high, iteration.high)
      }
      iteration.low <- cbind(rownames(iteration.stats$'lower level'),
                             iteration.stats$'lower level')
      rownames(iteration.low) <- NULL
      colnames(iteration.low)[1] <- "species"
      if (i == 1) { net.stats.low <- iteration.low } else {
        net.stats.low <- rbind(net.stats.low, iteration.low)
      }
      if (prog.count == TRUE){Sys.sleep(0.02); print(i); utils::flush.console()}
    }
    # --------------------------------------
    # --------------------------------------
    # Calculate the confidence limits and standardised effect sizes
    matrix.names <- colnames(net.stats.high)[2:ncol(net.stats.high)]
    higher.mean <- stats::aggregate(net.stats.high[, 2:ncol(net.stats.high)],
                             by = list(net.stats.high$species), mean,
                             na.rm = TRUE)
    higher.up <- stats::aggregate(net.stats.high[, 2:ncol(net.stats.high)],
                           by = list(net.stats.high$species), stats::quantile,
                           probs = 1 - p, na.rm = TRUE)
    higher.low <- stats::aggregate(net.stats.high[, 2:ncol(net.stats.high)],
                            by = list(net.stats.high$species), stats::quantile,
                            probs = p, na.rm = TRUE)
    higher.sd <- stats::aggregate(net.stats.high[, 2:ncol(net.stats.high)],
                           by = list(net.stats.high$species), stats::sd,
                           na.rm = TRUE)
    higher.ses <- cbind.data.frame(obs.high[, 1], ((obs.high[, -1] -
                                   higher.mean[, -1]) / higher.sd[, -1]))
    colnames(higher.mean) <- colnames(net.stats.high)
    colnames(higher.up) <- colnames(net.stats.high)
    colnames(higher.low) <- colnames(net.stats.high)
    colnames(higher.sd) <- colnames(net.stats.high)
    colnames(higher.ses) <- colnames(net.stats.high)
    # --------------------------------------
    # --------------------------------------
    # Lower level species may include taxa not present amongst the observed
    #   interactions e.g. if the abudance data include additional taxa. Hence
    #   an additional stage is added here to create a master list of taxa
    lower.taxa <- data.frame(Species = unique(c(as.character(obs.low$species),
                             as.character(net.stats.low$species))))
    obs.low <- merge(lower.taxa, obs.low, by.x = "Species", by.y = "species",
                    all.x = TRUE, all.y = TRUE)
    obs.low <- obs.low[order(obs.low$Species), ]
    lower.mean <- stats::aggregate(net.stats.low[, 2:ncol(net.stats.low)],
                            by = list(net.stats.low$species), mean, na.rm = TRUE)
    lower.mean <- merge(lower.taxa, lower.mean, by.x = "Species", by.y =
                        "Group.1",all.x = TRUE, all.y = TRUE)
    lower.mean <- lower.mean[order(lower.mean$Species), ]
    lower.up <- stats::aggregate(net.stats.low[, 2:ncol(net.stats.low)],
                          by = list(net.stats.low$species), stats::quantile,
                          probs = 1 - p, na.rm = TRUE)
    lower.up <- merge(lower.taxa, lower.up, by.x = "Species", by.y = "Group.1",
                      all.x = TRUE, all.y = TRUE)
    lower.up <- lower.up[order(lower.up$Species), ]
    lower.low <- stats::aggregate(net.stats.low[, 2:ncol(net.stats.low)],
                           by = list(net.stats.low$species), stats::quantile,
                           probs = p, na.rm = TRUE)
    lower.low <- merge(lower.taxa, lower.low, by.x = "Species", by.y = "Group.1",
                       all.x = TRUE, all.y = TRUE)
    lower.low <- lower.low[order(lower.low$Species), ]
    lower.sd <- stats::aggregate(net.stats.low[, 2:ncol(net.stats.low)],
                          by = list(net.stats.low$species), stats::sd, na.rm = TRUE)
    lower.sd <- merge(lower.taxa, lower.sd, by.x = "Species", by.y = "Group.1",
                    all.x = TRUE, all.y = TRUE)
    lower.sd <- lower.sd[order(lower.sd$Species), ]
    lower.ses <- cbind.data.frame(obs.low[, 1], ((obs.low[, -1] -
                                  lower.mean[, -1]) / lower.sd[, -1]))

    if(!identical(obs.low$Species, lower.mean$Species) ||
       !identical(obs.low$Species, lower.mean$Species) ||
       !identical(obs.low$Species, lower.up$Species) ||
       !identical(obs.low$Species, lower.sd$Species)) {stop(
       "Different taxon lists in observed and modelled data")}

    colnames(lower.mean) <- colnames(net.stats.high)
    colnames(lower.up) <- colnames(net.stats.high)
    colnames(lower.low) <- colnames(net.stats.high)
    colnames(lower.sd) <- colnames(net.stats.high)
    colnames(lower.ses) <- colnames(net.stats.high)
    # --------------------------------------
    # --------------------------------------
    # Error handling - covers the possibility that the set of indices differ
    #  between 'higher' and 'lower' levels in the web (in case some are
    #  level-specific) as this would compromise the output format
    if (!identical(as.vector(colnames(net.stats.high)[2:ncol(net.stats.high)]),
        as.vector(colnames(net.stats.high)[2:ncol(net.stats.high)])))
        {stop("Different indices at lower and higher levels")}
    # --------------------------------------
    # --------------------------------------
    # Set up output: an array of paired matrices ($higher and $lower - the levels
    #   of the web for which the statistics are calculated.
    sp.level <- list()
      for (j in 1:length(matrix.names)) {
      mat1 <- merge(obs.high[, c("species", matrix.names[j])],
                    higher.mean[, c("species", matrix.names[j])],
                    by.x = "species", by.y = "species")
      colnames(mat1) <- c("Species", "Observed", "Null")
      mat1 <- merge(mat1, higher.low[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      mat1 <- merge(mat1, higher.up[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      mat1 <- cbind(mat1, rep("ns", nrow(mat1)))
      mat1 <- merge(mat1, higher.ses[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      colnames(mat1)[4:7] <- c("Lower.CL", "Upper.CL", "Test", "SES")
      mat1$Test <- as.character(mat1$Test)
      for (k in 1:nrow(mat1)) {
        if (is.na(mat1$Observed[k])) {mat1$Test[k] <- "NA"} else {
          if ((mat1$Observed[k] == mat1$Lower.CL[k]) &
             (mat1$Observed[k] == mat1$Upper.CL[k])) {mat1$Test[k] <- "NA"}
          if (mat1$Observed[k] > mat1$Upper.CL[k]) {mat1$Test[k] <- "Higher"}
          if (mat1$Observed[k] < mat1$Lower.CL[k]) {mat1$Test[k] <- "Lower"}
        }
      }
      sp.level[[matrix.names[j]]]$higher <- mat1

      mat1 <- merge(obs.low[, c("Species", matrix.names[j])],
                    lower.mean[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      colnames(mat1) <- c("Species", "Observed", "Null")
      mat1 <- merge(mat1, lower.low[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      mat1 <- merge(mat1, lower.up[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      mat1 <- cbind(mat1, rep("ns", nrow(mat1)))
      mat1 <- merge(mat1, lower.ses[, c("species", matrix.names[j])],
                    by.x = "Species", by.y = "species")
      colnames(mat1)[4:7] <- c("Lower.CL", "Upper.CL", "Test", "SES")
      mat1$Test <- as.character(mat1$Test)
      for (k in 1:nrow(mat1)) {
        if (is.na(mat1$Observed[k]) || is.na(mat1$Lower.CL[k])) {mat1$Test[k] <- "NA"} else {
          if ((mat1$Observed[k] == mat1$Lower.CL[k]) &
              (mat1$Observed[k] == mat1$Upper.CL[k])) {mat1$Test[k] <- "ns"}
          if (mat1$Observed[k] > mat1$Upper.CL[k]) {mat1$Test[k] <- "Higher"}
          if (mat1$Observed[k] < mat1$Lower.CL[k]) {mat1$Test[k] <- "Lower"}
        }
      }
    sp.level[[matrix.names[j]]]$lower <- mat1
    }
  }
  # ======================================

  # --------------------------------------
  # Warning messages for unsupported indices
  if (index.type == "specieslevel" & indices == "ALL") {
    warning("Dependence matrix is not currently supported by bipartite_stats: indices = 'ALLBUTD' used instead")}
  if (index.type == "grouplevel" & indices == "ALL") {
    warning("Degree distribution is not currently supported by bipartite_stats: indices = 'ALLBUTDD' used instead")}
  if (index.type == "networklevel" & indices == "ALL") {
    warning("Degree distribution is not currently supported by bipartite_stats: indices = 'ALLBUTDD' used instead")}
  # Output
  if (index.type == "networklevel") {return(net.level.results)}
  if (index.type == "grouplevel")  {return(grp.level.results)}
  if (index.type == "specieslevel") {return(sp.level)}
}



