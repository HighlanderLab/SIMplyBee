% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L1_Pop.R
\name{getCastePop}
\alias{getCastePop}
\alias{getQueen}
\alias{getFathers}
\alias{getWorkers}
\alias{getDrones}
\alias{getVirginQueens}
\title{Access individuals of a caste}
\usage{
getCastePop(
  x,
  caste = "all",
  nInd = NULL,
  use = "rand",
  removeFathers = TRUE,
  collapse = FALSE,
  simParamBee = NULL
)

getQueen(x, collapse = FALSE, simParamBee = NULL)

getFathers(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL)

getWorkers(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL)

getDrones(
  x,
  nInd = NULL,
  use = "rand",
  removeFathers = TRUE,
  collapse = FALSE,
  simParamBee = NULL
)

getVirginQueens(
  x,
  nInd = NULL,
  use = "rand",
  collapse = FALSE,
  simParamBee = NULL
)
}
\arguments{
\item{x}{\code{\link{Colony-class}} or \code{\link{MultiColony-class}},
exceptionally \code{\link{Pop-class}} for calling \code{getFathers}
on a queen population}

\item{caste}{character, "queen", "fathers", "workers", "drones",
"virginQueens", or "all"}

\item{nInd}{numeric, number of individuals to access, if \code{NULL} all
individuals are accessed; if there are less individuals than requested,
we return the ones available - this can return \code{NULL}.
If input is \code{\link{MultiColony-class}},
the input could also be a vector of the same length as the number of colonies. If
a single value is provided, the same value will be applied to all the colonies.}

\item{use}{character, all options provided by \code{\link{selectInd}} and
\code{"order"} that selects \code{1:nInd} individuals (meaning it always
returns at least one individual, even if \code{nInd = 0})}

\item{removeFathers}{logical, removes \code{drones} that have already mated;
set to \code{FALSE} if you would like to get drones for mating with multiple
virgin queens, say via insemination}

\item{collapse}{logical, whether to return a single merged population}

\item{simParamBee}{\code{\link{SimParamBee}}, global simulation parameters}
}
\value{
when \code{x} is \code{\link{Colony-class}} return is
  \code{\link{Pop-class}} for \code{caste != "all"} or list for \code{caste
  == "all"} with nodes named by caste; when \code{x} is
  \code{\link{MultiColony-class}} return is a named list of
  \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
  \code{\link{Pop-class}} for \code{caste == "all"}. You can merge
  all the populations in the list with \code{\link{mergePops}} function.
}
\description{
Level 1 function that returns individuals of a caste. These
  individuals stay in the colony (compared to \code{\link{pullCastePop}}).
}
\section{Functions}{
\itemize{
\item \code{getQueen()}: Access the queen

\item \code{getFathers()}: Access fathers (drones the queen mated with)

\item \code{getWorkers()}: Access workers

\item \code{getDrones()}: Access drones

\item \code{getVirginQueens()}: Access virgin queens

}}
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

# Build-up and add virgin queens
colony <- buildUp(x = colony)
apiary <- buildUp(x = apiary)
colony <- addVirginQueens(x = colony)
apiary <- addVirginQueens(x = apiary)

# Get the queen of the colony
getCastePop(colony, caste = "queen")
getQueen(colony)

# Comparison of getCastePop() and getWorkers()
getCastePop(colony, caste = "workers")
getCastePop(colony, caste = "workers")
getCastePop(colony, caste = "workers", nInd = 2)
# Or aliases
getWorkers(colony)
# Same aliases exist for all the castes!

# Input is a MultiColony class - same behaviour as for the Colony!
getCastePop(apiary, caste = "queen")
# Or alias
getQueen(apiary)

# Sample individuals from all the castes
getCastePop(colony, nInd = 5, caste = "all")

# Get different number of workers per colony
getCastePop(apiary, caste = "workers", nInd = c(10, 20))
# Or alias
getWorkers(apiary, nInd = c(10, 20))

# Obtain individuals from MultiColony as a single population
getCastePop(apiary, caste = "queen", collapse = TRUE)
getQueen(apiary, collapse = TRUE)
getWorkers(apiary, nInd = 10, collapse = TRUE)
getDrones(apiary, nInd = 3, collapse = TRUE)
}
\seealso{
\code{\link{getQueen}}, \code{\link{getFathers}},
  \code{\link{getVirginQueens}}, \code{\link{getWorkers}}, and
  \code{\link{getDrones}}

\code{\link{getCasteId}} and \code{\link{getCaste}}
}
