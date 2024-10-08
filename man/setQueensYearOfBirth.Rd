% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L1_Pop.R
\name{setQueensYearOfBirth}
\alias{setQueensYearOfBirth}
\title{Set the queen's year of birth}
\usage{
setQueensYearOfBirth(x, year, simParamBee = NULL)
}
\arguments{
\item{x}{\code{\link[AlphaSimR]{Pop-class}} (one or more than one queen),
\code{\link[SIMplyBee]{Colony-class}} (one colony), or
\code{\link[SIMplyBee]{MultiColony-class}} (more colonies)}

\item{year}{integer, the year of the birth of the queen}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}
}
\value{
\code{\link[AlphaSimR]{Pop-class}}, \code{\link[SIMplyBee]{Colony-class}}, or
  \code{\link[SIMplyBee]{MultiColony-class}} with queens having the year of birth set
}
\description{
Level 1 function that sets the queen's year of birth.
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
colony <- cross(x = colony, drones = droneGroups[[1]])
apiary <- createMultiColony(basePop[3:4], n = 2)
apiary <- cross(apiary, drones = droneGroups[c(2, 3)])

# Example on Colony class
getQueenYearOfBirth(colony)
getQueenYearOfBirth(apiary)

queen1 <- getQueen(colony)
queen1 <- setQueensYearOfBirth(queen1, year = 2022)
getQueenYearOfBirth(queen1)

colony <- setQueensYearOfBirth(colony, year = 2022)
getQueenYearOfBirth(colony)

apiary <- setQueensYearOfBirth(apiary, year = 2022)
getQueenYearOfBirth(apiary)
}
