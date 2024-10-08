% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L0_auxilary.R
\name{getCaste}
\alias{getCaste}
\title{Report caste of an individual}
\usage{
getCaste(x, collapse = FALSE, simParamBee = NULL)
}
\arguments{
\item{x}{\code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
\code{\link[SIMplyBee]{MultiColony-class}}}

\item{collapse}{logical, if \code{TRUE}, the function will return a single
vector with caste information}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}
}
\value{
When x is \code{\link[AlphaSimR]{Pop-class}}, character of caste status; if you
  get \code{NA} note that this is not supposed to happen. When x is
  \code{\link[SIMplyBee]{Colony-class}}, list with character vectors (list is named with
  caste). When x is \code{\link[SIMplyBee]{MultiColony-class}}, list of lists with
  character vectors (list is named with colony id).
}
\description{
Level 0 function that reports caste of an individual
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
colony <- buildUp(x = colony, nWorkers = 20, nDrones = 5)
colony <- addVirginQueens(colony, nInd = 5)

apiary <- createMultiColony(basePop[3:4], n = 2)
apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
apiary <- buildUp(x = apiary, nWorkers = 10, nDrones = 2)
apiary <- addVirginQueens(apiary, nInd = 4)

getCaste(getQueen(colony))
getCaste(getFathers(colony))
getCaste(getWorkers(colony))
getCaste(getDrones(colony))
getCaste(getVirginQueens(colony))

bees <- c(
  getQueen(colony),
  getFathers(colony, nInd = 2),
  getWorkers(colony, nInd = 2),
  getDrones(colony, nInd = 2),
  getVirginQueens(colony, nInd = 2)
)
getCaste(bees)

getCaste(colony)
# Collapse information into a single vector
getCaste(colony, collapse = TRUE)
getCaste(apiary)

# Create a data.frame with id, colony, and caste information
(tmpC <- getCaste(apiary[[1]]))
(tmpI <- getCasteId(apiary[[1]]))
tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI))
head(tmp)
tail(tmp)

(tmpC <- getCaste(apiary))
(tmpI <- getCasteId(apiary))
(tmp <- data.frame(caste = unlist(tmpC), id = unlist(tmpI)))
tmp$colony <- sapply(
  X = strsplit(
    x = rownames(tmp), split = ".",
    fixed = TRUE
  ),
  FUN = function(z) z[[1]]
)
head(tmp)
tail(tmp)
}
\seealso{
\code{\link[SIMplyBee]{getCastePop}} and \code{\link[SIMplyBee]{getCasteId}}
}
