% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-SimParamBee.R
\name{mapCasteToColonyValue}
\alias{mapCasteToColonyValue}
\alias{mapCasteToColonyPheno}
\alias{mapCasteToColonyGv}
\alias{mapCasteToColonyBv}
\alias{mapCasteToColonyDd}
\alias{mapCasteToColonyAa}
\title{Map caste member (individual) values to a colony value}
\usage{
mapCasteToColonyValue(
  colony,
  value = "pheno",
  queenTrait = 1,
  queenFUN = function(x) x,
  workersTrait = 2,
  workersFUN = colSums,
  dronesTrait = NULL,
  dronesFUN = NULL,
  traitName = NULL,
  combineFUN = function(q, w, d) q + w,
  checkProduction = TRUE,
  notProductiveValue = 0,
  simParamBee = NULL
)

mapCasteToColonyPheno(colony, simParamBee = NULL, ...)

mapCasteToColonyGv(colony, simParamBee = NULL, ...)

mapCasteToColonyBv(colony, simParamBee = NULL, ...)

mapCasteToColonyDd(colony, simParamBee = NULL, ...)

mapCasteToColonyAa(colony, simParamBee = NULL, ...)
}
\arguments{
\item{colony}{\code{\link[SIMplyBee]{Colony-class}}}

\item{value}{character, one of \code{pheno} or \code{gv}}

\item{queenTrait}{numeric (column position) or character (column name),
trait(s) that represents queen's contribution to colony value(s); if
\code{NULL} then this contribution is 0; you can pass more than one trait
here, but make sure that \code{combineFUN} works with these trait dimensions}

\item{queenFUN}{function, function that will be applied to queen's value}

\item{workersTrait}{numeric (column position) or character (column name),
trait(s) that represents workers' contribution to colony value(s); if
\code{NULL} then this contribution is 0; you can pass more than one trait
here, but make sure that \code{combineFUN} works with these trait dimensions}

\item{workersFUN}{function, function that will be applied to workers values}

\item{dronesTrait}{numeric (column position) or character (column name),
trait(s) that represents drones' contribution to colony value(s); if
\code{NULL} then this contribution is 0; you can pass more than one trait
here, but make sure that \code{combineFUN} works with these trait dimensions}

\item{dronesFUN}{function, function that will be applied to drone values}

\item{traitName, }{the name of the colony trait(s), say, honeyYield; you can pass
more than one trait name here, but make sure to match them with
\code{combineFUN} trait dimensions}

\item{combineFUN, }{function that will combine the queen, worker, and drone
contributions - this function should be defined as \code{function(q, w, d)}
where \code{q} represents queen's, \code{q} represents workers', and
\code{d} represents drones' contribution.}

\item{checkProduction}{logical, does the value depend on the production
status of colony; if yes and production is \code{FALSE}, the return
is \code{notProductiveValue} - this will often make sense for colony
phenotype value only; you can pass more than one logical value here (one
per trait coming out of \code{combineFUN})}

\item{notProductiveValue}{numeric, returned value when colony is not productive;
you can pass more than one logical value here (one per trait coming out of
\code{combineFUN})}

\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}

\item{...}{other arguments of \code{mapCasteToColonyValue} (for its aliases)}
}
\value{
numeric matrix with one value or a row of values
}
\description{
Maps caste member (individual) values to a colony value - for
  phenotype, genetic, breeding, dominance, and epistasis values. This function
  can be used as \code{FUN} argument in \code{\link[SIMplyBee]{calcColonyValue}}
  function(s). It can also be saved in \code{SimParamBee$colonyValueFUN} as a
  default function called by \code{\link[SIMplyBee]{calcColonyValue}} function(s).

  This is just an example - quite a flexible one! You can provide your
  own "caste functions" that satisfy your needs within this mapping function
  (see \code{queenFUN}, \code{workersFUN}, and \code{dronesFUN} below)
  or provide a complete replacement of this mapping function! For example,
  this mapping function does not cater for indirect (social) genetic effects
  where colony individuals value impacts value of other colony individuals.
  Note though that you can achieve this impact also via multiple correlated
  traits, such as a queen and a workers trait.
}
\details{
This is a utility/mapping function meant to be called by
  \code{\link[SIMplyBee]{calcColonyValue}}. It only works on a single colony - use
  \code{\link[SIMplyBee]{calcColonyValue}} to get Colony or MultiColony values.
}
\section{Functions}{
\itemize{
\item \code{mapCasteToColonyPheno()}: Map caste member (individual) phenotype values to a colony phenotype value

\item \code{mapCasteToColonyGv()}: Map caste member (individual) genetic values to a colony genetic value

\item \code{mapCasteToColonyBv()}: Map caste member (individual) breeding values to a colony breeding value

\item \code{mapCasteToColonyDd()}: Map caste member (individual) dominance values to a colony dominance value

\item \code{mapCasteToColonyAa()}: Map caste member (individual) epistasis values to a colony epistasis value

}}
\examples{
founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}

# Define two traits that collectively affect colony honey yield:
# 1) queen's effect on colony honey yield, say via pheromone secretion phenotype
# 2) workers' effect on colony honey yield, say via foraging ability phenotype
# The traits will have a negative genetic correlation of -0.5 and heritability
# of 0.25 (on an individual level)
nWorkers <- 10
mean <- c(10, 10 / nWorkers)
varA <- c(1, 1 / nWorkers)
corA <- matrix(data = c(
  1.0, -0.5,
  -0.5, 1.0
), nrow = 2, byrow = TRUE)
varE <- c(3, 3 / nWorkers)
varA / (varA + varE)
SP$addTraitADE(nQtlPerChr = 100,
               mean = mean,
               var = varA, corA = corA,
               meanDD = 0.1, varDD = 0.2, corD = corA,
               relAA = 0.1, corAA = corA)
SP$setVarE(varE = varE)

basePop <- createVirginQueens(founderGenomes)
drones <- createDrones(x = basePop[1], nInd = 10)
colony <- createColony(x = basePop[2])
colony <- cross(colony, drones = drones)
colony <- buildUp(colony, nWorkers = nWorkers, nDrones = 3)

# Colony value
mapCasteToColonyPheno(colony)
mapCasteToColonyGv(colony)

# To understand where the above values come from, study the contents of
# mapCasteToColonyValue() and the values below:

# Phenotype values
getQueenPheno(colony)
getWorkersPheno(colony)

# Genetic values
getQueenGv(colony)
getWorkersGv(colony)

}
\seealso{
\code{\link[SIMplyBee]{SimParamBee}} field \code{colonyValueFUN} and functions
  \code{\link[SIMplyBee]{calcColonyValue}}, \code{\link[SIMplyBee]{calcColonyPheno}},
  \code{\link[SIMplyBee]{calcColonyGv}}, \code{\link[SIMplyBee]{getEvents}},
  \code{\link[AlphaSimR]{pheno}}, and \code{\link[AlphaSimR]{gv}}, as well as
  \code{vignette(topic = "QuantitativeGenetics", package = "SIMplyBee")}
}
