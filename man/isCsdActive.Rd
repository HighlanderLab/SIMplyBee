% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L0_auxilary.R
\name{isCsdActive}
\alias{isCsdActive}
\title{Is csd locus activated}
\usage{
isCsdActive(simParamBee = NULL)
}
\arguments{
\item{simParamBee}{\code{\link[SIMplyBee]{SimParamBee}}, global simulation parameters}
}
\value{
logical
}
\description{
Level 0 function that checks if the csd locus has been
  activated. See \code{\link[SIMplyBee]{SimParamBee}} for more information about the csd
  locus.
}
\examples{
founderGenomes <- quickHaplo(nInd = 3, nChr = 3, segSites = 100)
SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
\dontshow{SP$nThreads = 1L}
isCsdActive()

SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
isCsdActive()
}
