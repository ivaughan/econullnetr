#' Compare observed interaction strengths in a network to those estimated from
#'   a null model
#'
#' Takes the result of running a null model with \code{generate_null_net} and
#'   tests whether the observed interactions between consumer species and
#'   resource species differ those expected under the null model.
#'
#' @param nullnet An object of class "nullnet" from \code{generate_null_net}
#' @param signif.level An optional value specifying the threshold used for
#'   testing for 'significant' deviations from the null model.  Defaults to 0.95
#'
#' @details Statistical significance is determined for each consumer-resource
#'   interaction according to whether the observed interaction strength falls
#'   outside the confidence limits calculated across the iterations of the null
#'   model.  Confidence limits are calculated as the 1 -- alpha/2 percentiles from
#'   the frequency distribution (Manly 2006).
#'
#'   The observed and expected interactions strengths are also compared by
#'   calculating the standardised effect size  (Gotelli & McCabe 2002):
#'   \deqn{(observed link strength - expected link strength) / standard deviation
#'   of the link strength across the iterations of the null model}
#'
#'   \code{test_interactions} will issue warnings when:
#'   \enumerate{
#'     \item{The number of iterations of the null model was small <100, as
#'       the confidence intervals are unlikely to be reliable}
#'     \item{The number of tests >50, due to the increasing risk of Type I
#'       errors (incorrectly denoting an interaction as significantly different
#'       from the null model).  Many networks will contain many more than 100
#'       potential interactions, so the significance of individual interactions
#'       should be treated with caution.  Some form of false discovery rate
#'       correction may be valuable (e.g. the local false discovery rate;
#'       Gotelli & Ulrich 2010).}
#'   }
#'
#' @return Returns a data frame listing all possible consumer and resource
#'   species combinations with the following column headings:
#'   \describe{
#'     \item{\code{Consumer}}{The name of the consumer species}
#'     \item{\code{Resource}}{The name of the resource species}
#'     \item{\code{Observed}}{The 'strength' of the observed interaction (e.g. total number
#'       of interactions summed across the individual consumers)}
#'     \item{\code{Null}}{The mean strength of the interaction across the iterations of the
#'       null model}
#'     \item{\code{Lower}}{Lower confidence limit for the interaction strength}
#'     \item{\code{Upper}}{Upper confidence limit for the interaction strength}
#'     \item{\code{Test}}{Whether the observed interaction is significantly
#'       \strong{stronger} than expected under the null model, \strong{weaker}
#'       or consistent with the null model (\strong{ns})}
#'     \item{\code{SES}}{The standardised effect size for the interaction}
#'   }
#'
#' @seealso \code{\link{generate_null_net}}, \code{\link{plot_preferences}}
#'
#' @references Gotelli, N.J. & McCabe, D.J. (2002) Species co-occurrence:
#'    a meta-analysis of J. M. Diamond's assembly rules model. \emph{Ecology},
#'    \strong{83}, 2091--2096.
#'
#'    Gotelli, N.J. & Ulrich, W. (2010) The empirical Bayes approach as a tool
#'    to identify non-random species associations. \emph{Oecologia},
#'    \strong{162}, 463--477.
#'
#'    Manly, B.F.J. (2006) \emph{Randomization, Bootstrap and Monte Carlo
#'    Methods in Biology} (3rd edn). Chapman & Hall, Boca Raton.
#'
#'    Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E., Woodward, G. &
#'    Symondson, W.O.C. (2018) econullnetr: an R package using null models to
#'    analyse the structure of ecological networks and identify resource
#'    selection. \emph{Methods in Ecology and Evolution}, \strong{9}, 728--733.
#'
#' @examples
#' null.1 <- generate_null_net(WelshStreams[, 2:18], WelshStreams.prey[, 2:17],
#'                             sims = 10, c.samples = WelshStreams[, 1],
#'                             r.samples = WelshStreams.prey[, 1])
#' test_interactions(null.1, 0.95)
#'
#' @export



test_interactions <- function(nullnet, signif.level = 0.95) {
  # --------------------------------------
  # Return an error if a object from 'generate_null_net' is not supplied
  if(!inherits(nullnet, "nullnet")) stop(
    "test_interactions requires a nullnet object")

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
  # Convert to long table format using 'melt'
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
  for (i in 1:nrow(output)) {
    if (output$Observed[i] > output$Upper[i]) {output$Test[i] <- "Stronger"}
    if (output$Observed[i] < output$Lower[i]) {output$Test[i] <- "Weaker"}
  }
  # --------------------------------------

  # --------------------------------------
  # Calculate the standard effect sizes for observed v. null interactions, then
  #   remove the sd (no longer needed)
  output$SES <- with(output, (Observed - Null) / sd)
  output$SES[!is.finite(output$SES)] <- NA # Replaces Inf and NaN with NA
  output$sd <- NULL
  # --------------------------------------

  # --------------------------------------
  # Warning messages for i) Type I error and ii) small number of iterations
  if(nullnet$n.iterations < 100) warning(paste("Confidence limits will ",
     "be imprecise due to the small number of null model iterations", sep = ""))
  if(nrow(output) > 50) warning(paste("Be careful of Type I errors due to ",
     "the large number of tests", sep = ""))
  # --------------------------------------

  return(output)
}

