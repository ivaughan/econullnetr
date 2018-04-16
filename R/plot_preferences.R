#' Plot the resource preferences of a consumer
#'
#' Takes a 'nullnet' object from running a null model with
#'   \code{generate_null_net} and plots the observed and expected link
#'   strengths for every resource for a selected consumer species (node in
#'   the network).  There are two styles of plot: the default is a dot plot
#'   based on \code{\link[graphics]{dotchart}}, whilst the alternative is a
#'   bar plot based on \code{\link[graphics]{barplot}}.  There are arguments in
#'   \code{plot_preferences} to set some of the graphical parameters, but see
#'   the respective help files for \code{\link[graphics]{dotchart}} and
#'   \code{\link[graphics]{barplot}} for further options to customise the plots.
#'
#' @param nullnet An object of class nullnet from \code{generate_null_net}
#' @param node A string specifying the consumer node (species) whose
#'   preferences will be plotted
#' @param signif.level An optional value specifying the threshold used for
#'   testing for 'significant' deviations from the null model.  Defaults to 0.95
#' @param style An optional string to set whether a dotchart or bar chart is
#'   plotted. The default (\code{style = "dots"}) is a Cleveland dot plot,
#'   whilst the alternative (\code{style = "bars"}) is a bar chart of the type
#'   used by King \emph{et al.} (2010) and Davey \emph{et al.} (2013).
#' @param type Optional string to specify how preferences are displayed. The
#'   default (\code{type = "counts"}) is the total number of interactions or
#'   mean interaction strength, depending upon which one was requested in the
#'   original call to \code{\link{generate_null_net}} using the
#'   \code{summary.type} argument.  The alternative is the standardised effect
#'   size (\code{type = "SES"}).  With \code{type = "counts"},
#'   confidence limits will be drawn, whilst for the standardised effect
#'   size, dashed lines are added at +2 and -2, which is approximately equivalent
#'   to a 5\% significance level: fill colours for \code{type = "SES"} are
#'   based on the estimated confidence limits, rather than the +2/-2 thresholds.
#' @param res.col An optional character vector of length three specifying
#'   the colours with which to fill the dots or bars representing interactions
#'   that are weaker than expected, consistent with the null model and stronger
#'   than expected (in that order). The default is a red-blue colour scheme.
#' @param res.order An optional data frame used to set the order in which the
#'   resource species are plotted.  Should have two columns: the first listing
#'   the resource species (names must be identical to those used by
#'   \code{generate_null_net}) and the second the plotting order (bars will be
#'   plotted in ascending order)
#' @param l.cex An optional numeric value to set the size of the labels when
#'   using \code{style = "dots"}. Ignored when \code{style = "bars"}, as
#'   \code{barplot}'s \code{cex.names} argument can be used.
#' @param p.cex An optional numeric value to set the size of the points when
#'   when using \code{style = "dots"}.
#' @param ... Other arguments to control basic plotting functions in R, such as
#'   \code{lwd}, \code{xlab}, \code{ylab}, \code{main}, \code{xlim} and
#'   \code{ylim}.
#'
#' @details Plots the preferences for individual consumer species. The bar plot
#'   format follows the basic style of King \emph{et al}. (2010; Figure 3) and
#'   Davey \emph{et al.} (2013; Figure 3).
#'
#' @seealso \code{\link{generate_null_net}}, \code{\link{plot_bipartite}}
#'
#' @references Brewer, C.A. (2017) \url{http://www.ColorBrewer.org}
#'
#'   Davey, J.S., Vaughan, I.P., King, R.A., Bell, J.R., Bohan, D.A., Bruford,
#'   M.W., Holland, J.M. & Symondson, W.O.C. (2013) Intraguild predation in
#'   winter wheat: prey choice by a common epigeal carabid consuming spiders.
#'   \emph{Journal of Applied Ecology}, \strong{50}, 271--279.
#'
#'   King, R.A, Vaughan, I.P., Bell, J.R., Bohan, D.A, & Symondson, W.O.C.
#'   (2010) Prey choice by carabid beetles feeding on an earthworm community
#'   analysed using species- and lineage-specific PCR primers. \emph{Molecular
#'   Ecology}, \strong{19}, 1721--1732.
#'
#'   Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E., Woodward, G. &
#'   Symondson, W.O.C. (2018) econullnetr: an R package using null models to
#'   analyse the structure of ecological networks and identify resource
#'   selection. \emph{Methods in Ecology and Evolution}, \strong{9}, 728--733.
#'
#' @examples
#' null.1 <- generate_null_net(WelshStreams[, 2:18], WelshStreams.prey[, 2:17],
#'                             sims = 10, c.samples = WelshStreams[, 1],
#'                             r.samples = WelshStreams.prey[, 1])
#' # Basic plots, showing the dot and bar plot styles. Increased lower margin
#' #   on the bar plot so that names fit
#' plot_preferences(null.1, "Dinocras", signif.level = 0.95, type = "counts",
#'                  xlab = "Num. of visits", p.cex = 1.2, lwd = 2)
#'
#' par(mar = c(9, 4, 4, 2) + 0.1)
#' plot_preferences(null.1, "Dinocras", style = "bars", signif.level = 0.95,
#'                  type = "counts", ylab = "Num. of visits")
#'
#' # Same results, this time showing the standardised effect sizes
#' plot_preferences(null.1, "Rhyacophila", signif.level = 0.95,
#'                  type = "SES", xlab = "SES")
#' par(mar = c(9, 4, 4, 2) + 0.1)
#' plot_preferences(null.1, "Rhyacophila", signif.level = 0.95, style = "bars",
#'                  type = "SES", ylab = "SES")
#'
#' @export


plot_preferences <- function(nullnet, node, signif.level = 0.95,
                             style = "dots", type = "counts",
                             res.col = c("#67A9CF", "#F7F7F7", "#EF8A62"),
                             res.order = NULL, l.cex = 1, p.cex = 1, ...) {

  # Ensure 'res.order' is in data frame format (important when data are stored in
  #   tidyverse 'tibble' format)
  if(!is.null(res.order)) {
    res.order <- as.data.frame(res.order)
    res.order[, 1] <- as.factor(res.order[, 1])
  }
  # --------------------------------------
  # Set significance level
  if (signif.level <= 0 || signif.level >= 1) {
    stop("Invalid percentile value specified")}
  # --------------------------------------

  # --------------------------------------
  # Initial error handling:
  # 1. Return an error if a object from 'generate_null_net' is not supplied
  if(class(nullnet) != "nullnet") {
    stop("plot_preferences requires a nullnet object")}

  # 2. type <> "counts" or "SES", or style <> "dots" or "bars"
  if(type != "counts" && type != "SES") {
    stop("Invalid bar type specified")}
  if(style != "dots" && style != "bars") {
    stop("Invalid style of plot")}

  # 3. If a 'res.order' variable is specified, check that names match those
  #    in the nullnet object
  if (!is.null(res.order)) {
    if(!identical(sort(colnames(nullnet$rand.data[, 3:ncol(nullnet$rand.data)])),
                  as.character(sort(res.order[, 1])) )) {
      stop("Names in 'res.order' do not match resource names in nullnet object")}
  }
  # --------------------------------------

  # --------------------------------------
  # Call test_interactions
  ti <- test_interactions(nullnet, signif.level = signif.level)
  ti <- ti[ti$Consumer == node, ]
  ti[, 3] <- ifelse(rowSums(ti[, 3:6]) == 0, NA, ti[, 3])
  ti[, 4] <- ifelse(rowSums(ti[, 3:6]) == 0, NA, ti[, 4])
  ti[, 5] <- ifelse(rowSums(ti[, 3:6]) == 0, NA, ti[, 5])
  ti[, 6] <- ifelse(rowSums(ti[, 3:6]) == 0, NA, ti[, 6])
  # --------------------------------------

  # --------------------------------------
  # Plot style 1: dotchart
  # --------------------------------------
  if(style == "dots") {
    # Counts with confidence limits
    if(type == "counts") {
      # Set up maximum x-axis value for xlim. Add an additional 5%
      min.x <- min(ti[, 3:6], na.rm = TRUE)
      min.x <- max(0, min.x, na.rm = TRUE)
      max.x <- max(ti[, 3:6], na.rm = TRUE)
      max.x <- max.x * 1.05
      ti$Setup <- seq(min.x, max.x, length.out = nrow(ti))
      if (is.null(res.order)) {
        # Plot built up in 2 stages: i) using min and max values to set the
        #   y-axis range without having to use ylim (so this can be customised
        #   by the user), ii) the main dbarplot and label, and iii) the error
        graphics::dotchart(ti$Setup, labels = paste(ti$Resource, " ", sep = ""),
                           col = 1, pt.cex = 0, cex = l.cex, main = node, ...)
        graphics::abline(v = 0, lty = 2, col = "dimgrey")

        for (i in 1:nrow(ti)){
          eval(parse(text = paste("lines(x = c(ti$Lower.", signif.level * 100,
                                  ".CL[i], ti$Upper.", signif.level * 100,
                                  ".CL[i]), y = c(i, i), ...)", sep = "")))
          if(ti$Test[i] == "Weaker") p.col <- res.col[1]
          if(ti$Test[i] == "ns" | is.na(ti$Test[i])) p.col <- res.col[2]
          if(ti$Test[i] == "Stronger") p.col <- res.col[3]
          graphics::points(ti$Observed[i], i, pch = 21, col = "black",
                           bg = p.col, cex = p.cex)
        }
      } else {
        colnames(res.order) <- c("Taxon", "Order")
        ti <- merge(ti, res.order, by.x = "Resource", by.y = "Taxon")
        ti <- ti[order(ti$Order, decreasing = FALSE), ]
        graphics::dotchart(ti$Setup, labels = paste(ti$Resource, " ", sep = ""),
                           col = 1, pt.cex = 0, cex = l.cex, main = node, ...)
        graphics::abline(v = 0, lty = 2, col = "dimgrey")

        for (i in 1:nrow(ti)){
          eval(parse(text = paste("lines(x = c(ti$Lower.", signif.level * 100,
                                ".CL[i], ti$Upper.", signif.level * 100,
                                ".CL[i]), y = c(i, i), ...)", sep = "")))
          if(ti$Test[i] == "Weaker") p.col <- res.col[1]
          if(ti$Test[i] == "ns" | is.na(ti$Test[i])) p.col <- res.col[2]
          if(ti$Test[i] == "Stronger") p.col <- res.col[3]
          graphics::points(ti$Observed[i], i, pch = 21, col = "black",
                           bg = p.col, cex = p.cex)
        }
      }
    }
    # --------------------------------------

    # --------------------------------------
    # Plot type 2: Standardised effect sizes with 2 and -2 lines
    if(type == "SES") {
      # Set up maximum x-axis value for xlim. Add an additional 5%
      min.x <- min(ti$SES, na.rm = TRUE)
      max.x <- max(ti$SES, na.rm = TRUE)
      ran.x <- max(ti$SES, na.rm = TRUE) - min(ti$SES, na.rm = TRUE)
      min.x <- min(min.x - (ran.x * 0.05), 0)
      max.x <- max.x + (ran.x * 0.05)
      ti$Setup <- seq(min.x, max.x, length.out = nrow(ti))
      if (is.null(res.order)) {
        graphics::dotchart(ti$Setup, labels = paste(ti$Resource, " ", sep = ""),
                           col = 1, pt.cex = 0, cex = l.cex, main = node, ...)
        graphics::abline(v = 2, lty = 2)
        graphics::abline(v = 0, lty = 1)
        graphics::abline(v = -2, lty = 2)
        for (i in 1:nrow(ti)){
          if(ti$Test[i] == "Weaker") p.col <- res.col[1]
          if(ti$Test[i] == "ns" | is.na(ti$Test[i])) p.col <- res.col[2]
          if(ti$Test[i] == "Stronger") p.col <- res.col[3]
          graphics::points(ti$SES[i], i, pch = 21, col = "black",
                           bg = p.col, cex = p.cex)
        }
      } else {
        colnames(res.order) <- c("Taxon", "Order")
        ti <- merge(ti, res.order, by.x = "Resource", by.y = "Taxon")
        ti <- ti[order(ti$Order, decreasing = FALSE), ]

        graphics::dotchart(ti$Setup, labels = paste(ti$Resource, " ", sep = ""),
                           col = 1, pt.cex = 0, cex = l.cex, main = node, ...)
        graphics::abline(v = 2, lty = 2)
        graphics::abline(v = 0, lty = 1)
        graphics::abline(v = -2, lty = 2)
        for (i in 1:nrow(ti)){
          if(ti$Test[i] == "Weaker") p.col <- res.col[1]
          if(ti$Test[i] == "ns" | is.na(ti$Test[i])) p.col <- res.col[2]
          if(ti$Test[i] == "Stronger") p.col <- res.col[3]
          graphics::points(ti$SES[i], i, pch = 21, col = "black",
                           bg = p.col, cex = p.cex)
        }
      }
    }
  }

  # --------------------------------------
  # Plot style 2: barchart
  # --------------------------------------
  if(style == "bars") {
    # Vector of fill colours for the bars.
    if(!is.null(res.col)) {
      ti$Prefs <- rep(res.col[2], nrow(ti))
      for (i in 1:nrow(ti)) {
        if (ti$Test[i] == "Stronger") {ti$Prefs[i] <- res.col[3]}
        if (ti$Test[i] == "Weaker") {ti$Prefs[i] <- res.col[1] }
      }
    } else {
      ti$Prefs <- rep(0, nrow(ti))
    }
    # Counts with confidence limits
    if(type == "counts") {
      # Set up maximum y-axis value for ylim. Add an additional 5%
      min.y <- min(ti[, 3:6], na.rm = TRUE)
      min.y <- max(0, min.y, na.rm = TRUE)
      max.y <- max(ti[, 3:6], na.rm = TRUE)
      max.y <- max.y * 1.05
      ti$Setup <- seq(min.y, max.y, length.out = nrow(ti))
      if (is.null(res.order)) {
        # Plot built up in 3 stages: i) uses min and max values to set the
        #   y-axis range without having to use ylim (so this can be customised
        #   by the user), ii) the main barplot and label, and iii) the error
        #   bars using arrows. Warnings surpressed for arrows: warnings are
        #   generated when they are of 'zero length' i.e. no confidence interval
        bp1 <- graphics::barplot(ti$Setup, names.arg = ti$Resource,
                                 col = 0, border = NA, axisnames = FALSE, ...)
        graphics::barplot(ti$Observed, names.arg = ti$Resource, las = 3,
                          main = node, col = ti$Prefs, add = TRUE, ...)
        arrow.cols <- ifelse((ti[, 5] - ti[, 6]) == 0, NA, "black")
        graphics::arrows(bp1, ti[, 5], bp1, ti[, 6], lwd = 2,
                         col = arrow.cols, code = 3, angle = 90, length = .05)
      } else {
        colnames(res.order) <- c("Taxon", "Order")
        ti <- merge(ti, res.order, by.x = "Resource", by.y = "Taxon")
        ti <- ti[order(ti$Order, decreasing = FALSE), ]
        bp1 <- graphics::barplot(ti$Setup, names.arg = ti$Resource,
                                 col = 0, border = NA, axisnames = FALSE, ...)
        graphics::barplot(ti$Observed, names.arg = ti$Resource,
                          las = 3, main = node, col = ti$Prefs, add = TRUE, ...)
        arrow.cols <- ifelse((ti[, 5] - ti[, 6]) == 0, NA, "black")
        graphics::arrows(bp1, ti[, 5], bp1, ti[, 6], lwd = 2,
                         col = arrow.cols, code = 3, angle = 90, length = .05)
      }
    }
    # --------------------------------------

    # --------------------------------------
    # Plot type 2: Standardised effect sizes with 2 and -2 lines
    if(type == "SES") {
      min.y <- min(ti$SES, na.rm = TRUE)
      max.y <- max(ti$SES, na.rm = TRUE)
      ran.y <- max(ti$SES, na.rm = TRUE) - min(ti$SES, na.rm = TRUE)
      min.y <- min(min.y - (ran.y * 0.05), 0)
      max.y <- max.y + (ran.y * 0.05)
      ti$Setup <- seq(min.y, max.y, length.out = nrow(ti))
      if (is.null(res.order)) {
        bp1 <- graphics::barplot(ti$Setup, names.arg = ti$Resource, col = 0,
                                 border = NA, axisnames = FALSE, ...)
        graphics::barplot(ti$SES, names.arg = ti$Resource, las = 3,
                          add = TRUE, main = node, col = ti$Prefs, ...)
        graphics::abline(h = 2, lty = 2)
        graphics::abline(h = -2, lty = 2)
      } else {
        colnames(res.order) <- c("Taxon", "Order")
        ti <- merge(ti, res.order, by.x = "Resource", by.y = "Taxon")
        ti <- ti[order(ti$Order, decreasing = FALSE), ]
        bp1 <- graphics::barplot(ti$Setup, names.arg = ti$Resource, col = 0,
                                 border = NA, axisnames = FALSE, ...)
        graphics::barplot(ti$SES, names.arg = ti$Resource, las = 3, add = TRUE,
                          main = node, col = ti$Prefs, ...)
        graphics::abline(h = 2, lty = 2)
        graphics::abline(h = 0, lty = 1)
        graphics::abline(h = -2, lty = 2)
      }
    }
  }
}




