#' Null models for ecological networks
#'
#' Uses the algorithm of Agusti \emph{et al}. (2003) to specify and run a null
#'   model for an ecological network based upon interaction data and
#'   independent estimates of resource abundance (see Vaughan \emph{et al}.,
#'   2018 for full details).  Typically, network nodes represent species,
#'   and the following documentation uses the term species in place of node,
#'   but this need not be the case.
#'
#' @param consumers A matrix or data frame containing the interaction data. The
#'   first column should contain the name of the consumer species, with the
#'   remaining column names listing the resources (the names must be identical
#'   and in the same order as in \code{resources}).  Each row represents one
#'   individual, with the elements in the matrix indicating whether a resource
#'   was used or how much was used (see Details).
#' @param resources A matrix or data frame containing the relative abundances of
#'   the different resources (e.g. density of different prey species or abundance
#'   of different flower species).  May either have one row, if all data came
#'   from the same location and/or time, or have the same number of rows as
#'   there are sampling stratum codes in \code{r.samples} and \code{c.samples}
#'   (e.g. the set of time points or plot names), if the data are subdivided
#'   across sampling times/sites. Resource names (column names) in
#'   \code{resources} and \code{consumers} must be identical and in the same
#'   order.
#' @param sims Number of iterations of the null model.  The default value is 100,
#'   but this should generally be increased to estimate meaningful confidence
#'   limits.
#' @param data.type An optional string specifying the type of interaction data.
#'   One of three options: \code{"names"} (the default), \code{"counts"} or
#'   \code{"quantities"}. See Details for a full explanation and examples.
#' @param maintain.d When \code{data.type = "counts"} or \code{"quantities"},
#'   a string indicating whether the degree of each individual consumer (i.e.
#'   the number of different resource species it interacted with) should be
#'   maintained when allocating individual resources/quantities. Default is FALSE.
#' @param summary.type An optional string indicating how the interactions should
#'   be summarised at the species level: one of \code{"sum"} (the default),
#'   indicating that the interactions between a consumer and resource species
#'   will be summed across all individuals, or \code{"mean"}, indicating that
#'   the mean value per individual of a consumer species will be calculated.
#' @param c.samples An optional vector that specifies names for subdivisions of the
#'   interaction data when data on interactions and resource abundance were
#'   collected across a series of subdivisions (e.g. at different sites or time
#'   points). If suddivisions were present, they should be specified for both
#'   \code{c.samples} and \code{r.samples}.  The sample names must be identical
#'   in \code{c.samples} and \code{r.samples} (although \code{c.samples} is likely to
#'   be much longer than \code{r.samples} due to >1 individual consumer per
#'   subdivision)
#' @param r.samples An optional vector specifying the sample names for the
#'   resource abundance data (\code{resources}), and corresponding to the names
#'   used in \code{c.samples}. Must have the name number of entries as there are
#'   rows in \code{resources}. Not needed when \code{resources} contains a single
#'   row/\code{c.samples} is absent.
#' @param r.weights An optional data frame or matrix specifying weights to be
#'   applied to rows in \code{resources}: all entries must be in the range 0--1.
#'   The first column should contain the consumer species names, one entry for
#'   each species, within the remaining columns listing all species present in
#'   \code{resources} (names must be identical and in the same order as in
#'   \code{resources}).  Mainly used to specify forbidden links by specifying
#'   zero values for forbidden links and ones for the remaining entries.  Only
#'   one table is supplied, which then applies across all subdivisions of the
#'   data (if present).
#' @param prog.count A logical value specifying whether the progress count
#'   should be shown. Defaults to \code{TRUE}.
#'
#' @details A basic call to \code{generate_null_net} only needs two arguments:
#'   \code{consumers} and \code{resources}, but it is recommended that
#'   \code{sims} is also specified to run a larger number of iteration of the
#'   null model (e.g. 1000).  It is important to ensure that species names are
#'   consistent throughout and that the resource species used as column names in
#'   \code{resources}, \code{consumers} and (optionally) \code{r.weights} are in
#'    the same order (both \code{consumers} and \code{r.weights} should be one
#'   column wider than \code{resources}, because they include an extra column
#'   at the start to list the consumer species).
#'
#'   The same species can appear as both a consumer and a resource e.g. when a
#'   species is both predator and prey in a food web.  Interactions can be
#'   excluded from the modelled networks by specifying forbidden links with
#'   \code{r.weights}, based either on existing data/literature or hypothesed
#'   choices, generating the network that would be created if those choices were
#'   made.  Placing limits on the feasible range of resources with which a
#'   consumer interacts should lead to more realistic networks: great white
#'   sharks not feeding on zooplankton in a marine food web, for example.
#'
#'   The interactions between individual consumers and the resources may be
#'   recorded in a range of different ways depending upon the empirical data
#'   that are available, and these differences are handled by using the
#'   optional \code{data.type} argument.  Three types of data can be specified:
#'   \enumerate{
#'     \item \code{data.type = "names"} (the default value). This is the most
#'     common type of data, recording one or more resource species with which
#'     an interaction occurred, but without any attempt to quantify the
#'     strength of the interaction.  For data of this type, \code{resources}
#'     should simply comprise 1s and 0s, indicating whether an interaction was
#'     present or absent respectively: row sums may equal one or be >1 if
#'     individual consumers can interact with multiple resources.
#'     \item \code{data.type = "counts"}.  These data record the number of times
#'     an individual consumer interacted with different resource species e.g.
#'     the number of times different flower species were visited by an
#'     individual pollinator during a 5 minute observation period.  When
#'     modelling count data there is a choice of whether the total number of
#'     interactions (an individual's row sum) can be distributed across all
#'     potential resources, which may result in interactions with a different
#'     number of resource species (i.e. a change in an individual's degree) or
#'     whether the degree is held constant for each individual.  This is
#'     determined by the additional \code{maintain.d} argument:
#'     \code{maintain.d = FALSE} (the default) does not constrain an
#'     individual's degree, whereas \code{maintain = TRUE} does.
#'     \item \code{data.type = "quantities"}. Quantitative data are obtained
#'     from each individual, such as the proportion of a predator's gut
#'     contents derived from different prey.  The data in \code{consumers} can
#'     be represented by either proportions (so that the row sum for an
#'     individual = 1) or in the native units (so the total <> 1). As for count
#'     data, the degree can be free or fixed at the individual consumer level
#'     by using the \code{maintain.d} argument.
#'   }
#'  For data types 2 and 3, it does not matter what units are used (e.g. each
#'  row does not need to add up to one).  The row total will be maintained,
#'  so the results are returned in the same units as the original data.
#'
#'  One problem that may arise in specifying the null model is in situations
#'  where an interaction is recorded with a particular resource, but that
#'  resource was not actually detected in the abundance data (i.e. abundance =
#'  0).  This may occur, for example, when a predator eats a rare species that
#'  was missed during concomitant sampling of prey abundance: in effect, the
#'  predator's 'sampling' was more comprehensive.  The implication of this is
#'  that the resource species will not be sampled in the null model -- a
#'  potential source of bias.  \code{generate_null_net} will issue a warning if
#'  it detects this situation.  Possible corrective actions include removing
#'  that resource species altogether or replacing its zero abundance with a
#'  small constant.
#'
#' @return Returns an object of class "nullnet", which is a list containing
#'  the following components:
#'  \describe{
#'    \item{\code{rand.data}}{Data frame containing the results from all of the
#'      iterations of the null model}
#'    \item{\code{obs.interactions}}{Interaction matrix summarising the observed
#'      interactions (from \code{consumers})}
#'    \item{\code{n.iterations}}{The value of \code{sims} i.e. the number of
#'      interations of the null model}
#'  }
#'
#' @seealso \code{\link{test_interactions}}, \code{\link{plot_preferences}}
#'
#' @references Agusti, N., Shayler, S.P., Harwood, J.D., Vaughan, I.P.,
#'   Sunderland, K.D. & Symondson, W.O.C. (2003) Collembola as alternative prey
#'   sustaining spiders in arable ecosystems: prey detection within predators
#'   using molecular markers. \emph{Molecular Ecology}, \strong{12}, 3467--3475.
#'
#'   Davey, J.S., Vaughan, I.P., King, R.A., Bell, J.R., Bohan, D.A.,
#'   Bruford, M.W., Holland, J.M. & Symondson, W.O.C. (2013) Intraguild
#'   predation in winter wheat: prey choice by a common epigeal carabid
#'   consuming spiders. \emph{Journal of Applied Ecology}, \strong{50},
#'   271--279.
#'
#'   King, R.A, Vaughan, I.P., Bell, J.R., Bohan, D.A, & Symondson, W.O.C.
#'   (2010) Prey choice by carabid beetles feeding on an earthworm community
#'   analysed using species- and lineage-specific PCR primers. \emph{Molecular
#'   Ecology}, \strong{19}, 1721--1732.
#'
#'   Pearson, C.E., Symondson, W.O.C., Clare, E.L., Ormerod, S.J.,
#'   Iparraguirre Bolanos, E. & Vaughan, I.P. (2018) The effects of
#'   pastoral intensification on the feeding interactions of generalist
#'   predators in streams. \emph{Molecular Ecology}, \strong{27}, 590-602.
#'
#'   Vaughan, I.P., Gotelli, N.J., Memmott, J., Pearson, C.E., Woodward, G. &
#'   Symondson, W.O.C. (2018) econullnetr: an R package using null models to
#'   analyse the structure of ecological networks and identify resource
#'   selection. \emph{Methods in Ecology and Evolution}, \strong{9}, 728--733.
#'
#' @examples
#' null.1 <- generate_null_net(Silene[, 2:7], Silene.plants[, 2:6], sims = 10,
#'   data.type = "names", summary.type = "sum", c.samples = Silene[, 1],
#'   r.samples = Silene.plants[, 1])
#'
#' @export



generate_null_net <- function(consumers, resources, sims = 100,
                              data.type = "names", maintain.d = NULL,
                              summary.type = "sum", c.samples = NULL,
                              r.samples = NULL, r.weights = NULL,
                              prog.count = TRUE){

  # Ensure input data are in data frame format (important when data are stored
  #   in tidyverse 'tibble' format)
  consumers <- as.data.frame(consumers)
  consumers[, 1] <- as.factor(consumers[, 1])
  resources <- as.data.frame(resources)
  if(!is.null(c.samples)) c.samples <- as.data.frame(c.samples)[, 1]
  if(!is.null(r.samples)) r.samples <- as.data.frame(r.samples)[, 1]
  if(!is.null(r.weights)) r.weights <- as.data.frame(r.weights)

  # --------------------------------------
  # Initial error handling:
  # --------------------------------------
  # 1. Column names are identical (names and order):
  if (!identical(colnames(consumers)[-1], colnames(resources))) {
      stop("Resource names do not match in 'consumers' and 'resources'")}
  if(!is.null(r.weights) && (!identical(colnames(r.weights)[-1],
      colnames(resources)))) {
    stop("Names of 'r.weights' do not match names of 'resources'")}

  # 2. Either both or neither of r.samples and c.samples are present
  if (sum(is.null(r.samples), is.null(c.samples)) == 1) {
      stop("Only one of 'r.samples' and 'c.samples' supplied")}

  # 3. Check resource abundance data: length and sample codes
  # When sample codes are not present, 'resources' should be a single row
  if (is.null(r.samples) && (nrow(resources) != 1)) {
      stop("Resource abundances should have one row")}

  # When sample codes are present, the the length of 'c.samples' and 'r.samples'
  #   should match the number of rows in 'consumers' and 'resources' respectively,
  #   and the factor levels should be the same
  if(!is.null(r.samples) && {
     (nrow(resources) != length(r.samples)) ||
     (nrow(consumers) != length(c.samples)) ||
     (!identical(sort(unique(r.samples)), sort(unique(c.samples))))}) {
      stop("There is a problem with the sample codes:
'c.samples' and/or 'r.samples'
       are of incorrect length or their sample codes do not match")}

  # 4. Abundance weights (typically forbidden link values) are in the range 0 - 1
  if(!is.null(r.weights)) {
     if(max(r.weights[, -1]) > 1 || min(r.weights[, -1]) < 0) {
      stop("Abundance weights ('r.weights') must be between 0 and 1")}}

  # 5. Issue a warning (cf. error) if a resource is consumed where the recorded
  #    abundance of the resource = 0 i.e. a resource that cannot be consumed in
  #    the null model. This may or may not require action by the user.
  if (!is.null(c.samples)) {
    spp.consumed <- stats::aggregate(consumers[, -1], by = list(c.samples), sum)
    spp.consumed <- spp.consumed[order(spp.consumed$Group.1, decreasing = FALSE), ]
    all.res <- cbind(r.samples, resources)
    all.res <- all.res[order(all.res$r.samples, decreasing = FALSE), ]
    if(sum(spp.consumed[, -1] >0 && all.res[, -1] == 0) > 0) {warning(
      "One or more instances detected where a consumer interacted with a
       resource that has zero abundance in 'resources'")}
  } else {
    spp.consumed <- colSums(consumers[, -1])
    all.res <- resources
    if(sum(spp.consumed >0 & all.res == 0) > 0) {warning(
      "There is at least one instance where a resource was consumed but
      was zero in the abundance data (i.e. 'resources')")}
  }

  # 6. Correct types of data for data.type = "names" and "counts"
  if(data.type == "names" && {sum(consumers[, -1] == 1 | consumers[, -1] == 0) !=
     (nrow(consumers) * ncol(consumers[, -1]))}) {
     stop("Entries in the consumer data should equal 0 or 1")}
  if(data.type == "counts" && {sum(consumers[, -1] - round(consumers[, -1], 0)) > 0}) {
     stop("Entries in the consumer data should be integers")}
  # --------------------------------------

  # --------------------------------------
  # Set up a data frame that summarises the consumer data, containing:
  #   1. The original order of the data; 2. Consumer species; 3. Number of
  #      interactions per individual and total interactions (sum); 4. Sample
  #     codes (or "A" if samples not specified)
  if (!is.null(c.samples)){
    c.summary <- data.frame(ord = seq(1, nrow(consumers)), c.sample = c.samples,
                            species = consumers[, 1],
                            links = rowSums(consumers[, 2:ncol(consumers)] != 0),
                            total = rowSums(consumers[, 2:ncol(consumers)]))
  } else {
    c.summary <- data.frame(ord = seq(1, nrow(consumers)),
                            c.sample = rep("A", nrow(consumers)),
                            species = consumers[, 1],
                            links = rowSums(consumers[, 2:ncol(consumers)] != 0),
                            total = rowSums(consumers[, 2:ncol(consumers)]))
  }
  # --------------------------------------

  # --------------------------------------
  # Create data frame 'rd' by adding sample codes, where specified, to the
  #   resource abundance data. Otherwise a null sample value 'A' is added
  if (!is.null(c.samples)){
    rd <- cbind.data.frame(r.samples, resources)
    colnames(rd)[1] <- "r.sample"
  } else {
    rd <- cbind.data.frame(rep("A", nrow(resources)), resources)
    colnames(rd)[1] <- "r.sample"
  }
  # --------------------------------------

  # --------------------------------------
  #   1. Add in the summary data for individual consumers ('c.summary')
  #   2. Multiply the resource abundances by the r.weights matrix (if specified)
  rd2 <- merge(c.summary, rd, by.x = "c.sample", by.y = "r.sample")
  rd2 <- rd2[order(rd2$ord), ]
  if (!is.null(r.weights)) {
    abund.wghts <- merge(c.summary, r.weights, by.x = "species",
                         by.y = colnames(r.weights[1]))
    abund.wghts <- abund.wghts[order(abund.wghts$ord), ]

  # Error handling - check:
  #  1. The columns of consumer names match in the resource abundance 'rd2' and
  #      abundance weights 'abund.wghts' data frames
  #  2. The resource names and order match in 'rd2' and 'abund.wghts' before
  #      multiplying them together.
  if (!identical(abund.wghts$species, rd2$species))
      {stop("Consumer names in 'r.weights' and 'resources' do not match")}
  if (!identical(colnames(abund.wghts[, 6:ncol(abund.wghts)]),
                 colnames(rd2[6:ncol(rd2)])))
     {stop("Resource names in 'r.weights' and 'resources' do not match")}

  rd2[, 6:ncol(rd2)] <- rd2[, 6:ncol(rd2)] * abund.wghts[, 6:ncol(abund.wghts)]
  }

  # Error handling: check that the number of potential resources => the maximum
  #   number of interactions of individual consumers once any forbidden links
  #   are applied (within each sub-division of the data, where present)
  max.n.res <- data.frame(Subdiv = paste(rd2[, 1], rd2[, 3], sep = ""),
                          Links = rd2$links,
                          Nonzero = rowSums(rd2[, -c(1:5)] != 0))
  max.n.res <- stats::aggregate(max.n.res[, 2:3], by = list(max.n.res$Subdiv), max)
  if(sum(max.n.res$Links > max.n.res$Nonzero) > 0) {stop(
     "Some consumers interact with more resources than are available (>0 abundance)
     in 'resources', so the network cannot be modelled correctly")}
  # --------------------------------------

  # ======================================
  # Outer loop of the null model
  for (h in 1:sims){
    # Create a data frame to contain the simulated data
    res.df <- rd2[, 3:ncol(rd2)]
    res.df[, 4:ncol(res.df)] <- 0

    # -------------------------
    # 1. Qualitative (0/1 data)
    if (data.type == "names") {
      for (i in 1:nrow(res.df)) {
        res.choice <- sample(colnames(rd2[, 6:ncol(rd2)]), size = rd2$links[i],
                             prob = rd2[i, 6:ncol(rd2)], replace = FALSE)
        res.df[i, res.choice] <- 1
      }
    }

    # -------------------------
    # 2. Count data
    if (data.type == "counts") {
      if (is.null(maintain.d) | FALSE) { # Not maintaining the degree
        for (i in 1:nrow(res.df)) {
          res.choice <- sample(colnames(rd2[, 6:ncol(rd2)]), size = rd2$total[i],
                               prob = rd2[i, 6:ncol(rd2)], replace = TRUE)
          pc <- data.frame(table(res.choice))
          rownames(pc) <- pc$res.choice
          res.df[i, rownames(pc)] <- pc[rownames(pc), 2]
        }
      }
      if (!is.null(maintain.d) && TRUE) { # Maintaining the degree - 2 stage process
        for (i in 1:nrow(res.df)) {
          # Pt 1 - select the resource taxa
          choice <- sample(colnames(rd2[, 6:ncol(rd2)]), size = rd2$links[i],
                           prob = rd2[i, 6:ncol(rd2)], replace = FALSE)
          choice <- data.frame(Taxon = choice, pt1 = 1)
          # Pt 2 - allocate additional interations so the modelled total = observed total
          choice.2 <- sample(choice$Taxon, size = rd2$total[i] - rd2$links[i],
                     replace = TRUE)
          choice.2 <- data.frame(table(choice.2))
          choice <- merge(choice, choice.2, by.x = "Taxon", by.y = "choice.2")
          choice$count <- rowSums(choice[, -1])
          rownames(choice) <- choice$Taxon
         res.df[i, rownames(choice)] <- choice[rownames(choice), 4]
        }
      }
    }

    # -------------------------
    # 3. Measured quantities
    if (data.type == "quantities") {
      if (is.null(maintain.d) | FALSE) { # Not maintaining the degree
        for (i in 1:nrow(res.df)) {
          resource.props <- as.matrix(rd2[i, 6:ncol(rd2)] /
                                      sum(rd2[i, 6:ncol(rd2)]))
          res.choice <- gtools::rdirichlet(1, resource.props)
          res.choice <- res.choice * rd2[i, "total"]
          res.df[i, 4:ncol(res.df)] <- res.choice
        }
      }
      if (!is.null(maintain.d) && TRUE) { # Maintaining the degree - 2 stage process
        for (i in 1:nrow(res.df)) {
          # Pt 1 - select the resource taxa
          choice <- sample(colnames(rd2[, 6:ncol(rd2)]), size = rd2$links[i],
                           prob = rd2[i, 6:ncol(rd2)], replace = FALSE)
          choice <- data.frame(Taxon = choice, pt1 = 1)
          # Pt 2 - allocate interations among the selected taxa
          rp <- as.matrix(rd2[i, as.character(choice$Taxon)])
          colnames(rp) <- choice$Taxon
          resource.props <- as.matrix(rp / sum(rp))
          repeat{
            res.choice <- gtools::rdirichlet(1, resource.props)
            if(min(res.choice) > 0) break
          }
          colnames(res.choice) <- colnames(rp)
          res.choice <- res.choice * rd2[i, "total"]
          res.df[i, colnames(res.choice)] <- res.choice[, colnames(res.choice)]
        }
      }
    }
    # --------------------------------------

    # --------------------------------------
    # Prepare output from the iteration of the model (iterations indexed by
    #   the value of h)
    if (summary.type == "sum") {
      res.agg <- stats::aggregate(res.df[, 4:ncol(res.df)],
                                  by = list(res.df$species), sum)
      colnames(res.agg)[1] <- "Consumer"
      res.agg <- cbind(Iteration = as.matrix(rep(paste("It.", h, sep=""),
                                             length(res.agg[, 1]))), res.agg)
      ifelse(h == 1, res.compiled <- res.agg, {
             res.compiled <- rbind(res.compiled, res.agg)})
    }

    if (summary.type == "mean") {
      res.agg <- stats::aggregate(res.df[, 4:ncol(res.df)],
                                  by = list(res.df$species), mean)
      colnames(res.agg)[1] <- "Consumer"
      res.agg <- cbind(Iteration = as.matrix(rep(paste("It.", h, sep=""),
                                             length(res.agg[, 1]))), res.agg)
      ifelse(h == 1, {res.compiled <- res.agg}, {
                      res.compiled <- rbind(res.compiled, res.agg)})
    }
    # --------------------------------------

    # Progress count
    if (prog.count == TRUE){Sys.sleep(0.02); print(h); utils::flush.console()}
  }

  # --------------------------------------
  # Compile output objects:
  # 1. Observed consumer-resource interactions
  # 2. Null model outputs

  if (summary.type == "sum") {
    obs.interactions <- stats::aggregate(consumers[, 2:ncol(consumers)],
                                         list(consumers[, 1]), sum)
    colnames(obs.interactions)[1] <- "Consumer"
    null.interactions <- stats::aggregate(res.compiled[, 3:ncol(res.compiled)],
                                          list(res.compiled$Consumer), mean)
    colnames(null.interactions)[1] <- "Consumer"
    if(!identical(colnames(obs.interactions), colnames(null.interactions)) |
       !identical(obs.interactions$Consumer, null.interactions$Consumer))
       {stop("Different observed and expected taxon names")}
  }

  if (summary.type == "mean") {
    obs.interactions <- stats::aggregate(consumers[, 2:ncol(consumers)],
                                         list(consumers[, 1]), mean)
    colnames(obs.interactions)[1] <- "Consumer"
    null.interactions <- stats::aggregate(res.compiled[, 3:ncol(res.compiled)],
                                          list(res.compiled$Consumer), mean)
    colnames(null.interactions)[1] <- "Consumer"
    if(!identical(colnames(obs.interactions), colnames(null.interactions) )|
       !identical(obs.interactions$Consumer, null.interactions$Consumer))
       {stop("Different observed and expected names")}
  }
  # --------------------------------------

  # --------------------------------------
  # Create output list
  null.net.res <- list(rand.data = res.compiled, obs.interactions = obs.interactions,
       n.iterations = sims)
  class(null.net.res) <- "nullnet"
  return(null.net.res)
  # --------------------------------------

}

