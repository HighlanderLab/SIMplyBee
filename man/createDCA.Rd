% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L1_Pop.R
\name{createDCA}
\alias{createDCA}
\title{Create a drone congregation area (DCA)}
\usage{
createDCA(x, nInd = NULL, removeFathers = TRUE, simParamBee = NULL)
}
\arguments{
\item{x}{\code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}}

\item{nInd}{numeric, number of random drones to pull from each colony,
if \code{NULL} all drones in a colony are pulled}

\item{removeFathers}{logical, removes \code{drones} that have already mated;
set to \code{FALSE} if you would like to get drones for mating with multiple
virgin queens, say via insemination}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}
}
\value{
\code{\link[AlphaSimR]{Pop-class}}
}
\description{
Level 1 function that creates a population of drones from a Colony
  or MultiColony.  Such a population is often referred to as a drone
  congregation area (DCA).
}
\details{
In reality, drones leave the colony to mate. They die after that.
  In this function we only get a copy of drones from \code{x}, for
  computational efficiency and ease of use. However, any mating will change
  the caste of drones to fathers, and they won't be available for future
  matings (see \code{\link[SIMplyBee]{cross}}). Not unless
  \code{removeFathers = FALSE}.
}
\examples{
founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
basePop <- createVirginQueens(founderGenomes)

drones <- createDrones(x = basePop[1], nInd = 1000)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)

# Create a Colony and a MultiColony class
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = droneGroups[[1]])
apiary <- createMultiColony(basePop[3:4], n = 2)
apiary <- cross(apiary, drones = droneGroups[c(2, 3)])

colony <- addDrones(colony, nInd = 10)
createDCA(colony)
createDCA(colony, nInd = 10)@id

apiary <- addDrones(apiary)
createDCA(apiary)
createDCA(apiary, nInd = 10)
}
