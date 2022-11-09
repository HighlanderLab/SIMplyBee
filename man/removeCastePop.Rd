% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L2_Colony.R
\name{removeCastePop}
\alias{removeCastePop}
\alias{removeQueen}
\alias{removeWorkers}
\alias{removeDrones}
\alias{removeVirginQueens}
\title{Remove a proportion of caste individuals from a colony}
\usage{
removeCastePop(x, caste = NULL, p = 1, use = "rand")

removeQueen(x)

removeWorkers(x, p = 1, use = "rand")

removeDrones(x, p = 1, use = "rand")

removeVirginQueens(x, p = 1, use = "rand")
}
\arguments{
\item{x}{\code{\link{Colony-class}} or \code{\link{MultiColony-class}}}

\item{caste}{character, "queen", "workers", "drones", or "virginQueens"}

\item{p}{numeric, proportion to be removed; if input is \code{\link{MultiColony-class}},
the input could also be a vector of the same length as the number of colonies. If
a single value is provided, the same value will be applied to all the colonies}

\item{use}{character, all the options provided by \code{\link{selectInd}} -
guides selection of virgins queens that will stay when \code{p < 1}}
}
\value{
\code{\link{Colony-class}} or \code{\link{MultiColony-class}} without virgin queens
}
\description{
Level 2 function that removes a proportion of virgin queens of
  a Colony or MultiColony object
}
\section{Functions}{
\itemize{
\item \code{removeQueen()}: Remove queen from a colony

\item \code{removeWorkers()}: Remove workers from a colony

\item \code{removeDrones()}: Remove workers from a colony

\item \code{removeVirginQueens()}: Remove virgin queens from a colony

}}
\examples{
founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
basePop <- createVirginQueens(founderGenomes)

drones <- createDrones(x = basePop[1], nInd = 100)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)

# Create and cross Colony and MultiColony class
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = droneGroups[[1]])
apiary <- createMultiColony(basePop[4:5], n = 2)
apiary <- cross(apiary, drones = droneGroups[3:4])

# Add virgin queens
colony <- addVirginQueens(colony, nInd = 10)
apiary <- addVirginQueens(apiary, nInd = 10)

# Remove virgin queens
nVirginQueens(colony)
colony <- removeCastePop(colony, caste = "virginQueens", p = 0.5)
# or alias: removeVirginQueens(colony, p = 0.5)
nVirginQueens(colony)
colony <- removeCastePop(colony, caste = "virginQueens")
# or alias: colony <- removeVirginQueens(colony)
nVirginQueens(colony)
nWorkers(colony)
nDrones(colony)
colony <- removeCastePop(colony, caste = "workers", p = 0.3)
# or alias: colony <- removeWorkers(colony, p = 0.3)
colony <- removeCastePop(colony, caste = "drones", p = 0.3)
# or alias: colony <- removeDrones(colony, p = 0.3)

nVirginQueens(apiary)
nWorkers(apiary)
nDrones(apiary)
apiary <- removeCastePop(apiary, caste = "virginQueens", p = 0.3)
# or alias: removeVirginQueens(apiary, p = 0.3)
nVirginQueens(apiary)
apiary <- removeCastePop(apiary, caste = "workers", p = 0.3)
# or alias: removeWorkers(apiary, p = 0.3)
nWorkers(apiary)
#' apiary <- removeCastePop(apiary, caste = "drones", p = 0.3)
# or alias: removeDrones(apiary, p = 0.3)
nDrones(apiary)
# Remove different proportions
apiary <- buildUp(apiary)
nWorkers(apiary); nDrones(apiary)
nWorkers(removeWorkers(apiary, p = c(0.1, 0.5)))
nDrones(removeDrones(apiary, p = c(0.1, 0.9)))
}