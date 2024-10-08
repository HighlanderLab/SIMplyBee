% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L2_Colony.R
\name{resetEvents}
\alias{resetEvents}
\title{Reset colony events}
\usage{
resetEvents(x, collapse = NULL)
}
\arguments{
\item{x}{\code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}}}

\item{collapse}{logical, reset the collapse event (only sensible in setting
up a new colony, which the default of \code{NULL} caters for; otherwise, a
collapsed colony should be left collapsed forever, unless you force
resetting this event with \code{collapse = TRUE})}
}
\value{
\code{\link[SIMplyBee]{Colony-class}} or \code{\link[SIMplyBee]{MultiColony-class}} with
  events reset
}
\description{
Level 2 function that resets the slots swarm, split,
  supersedure, collapsed, and production to FALSE in a Colony or MultiColony object.
  Useful at the end of a yearly cycle to reset the events, allowing the user to track
  new events in a new year.
}
\examples{
founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 50)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
basePop <- createVirginQueens(founderGenomes)

drones <- createDrones(x = basePop[1], nInd = 100)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 5, nDrones = nFathersPoisson)

# Create and cross Colony and MultiColony class
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = droneGroups[[1]])
apiary <- createMultiColony(basePop[4:5], n = 2)
apiary <- cross(apiary, drones = droneGroups[3:4])

# Build-up - this sets Productive to TRUE
(colony <- buildUp(colony, nWorkers = 100))
isProductive(colony)
resetEvents(colony)

apiary <- buildUp(apiary, nWorkers = 100)
isProductive(apiary)
resetEvents(apiary)

# Split - this sets Split to TRUE
tmp <- split(colony)
(split <- tmp$split)
hasSplit(split)
resetEvents(split)
(remnant <- tmp$remnant)
hasSplit(remnant)
resetEvents(remnant)

# Swarm - this sets Swarm to TRUE
tmp <- swarm(colony)
(swarm <- tmp$swarm)
hasSwarmed(swarm)
resetEvents(swarm)
(remnant <- tmp$remnant)
hasSwarmed(remnant)
resetEvents(remnant)

# Supersede - this sets Supersede to TRUE
(tmp <- supersede(colony))
hasSuperseded(tmp)
resetEvents(tmp)

# Collapse - this sets Collapse to TRUE
(tmp <- collapse(colony))
hasCollapsed(tmp)
resetEvents(tmp)
resetEvents(tmp, collapse = TRUE)

# Same behaviour for MultiColony (example for the split)
tmp <- split(apiary)
(splits <- tmp$split)
hasSplit(splits[[1]])
resetEvents(splits)[[1]]
(remnants <- tmp$remnant)
hasSplit(remnants[[1]])
resetEvents(remnants)[[1]]
}
