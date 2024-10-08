% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions_L0_auxilary.R
\name{calcBeeGRMIbs}
\alias{calcBeeGRMIbs}
\alias{calcBeeAlleleFreq}
\title{Calculate Genomic Relatedness Matrix (GRM) for honeybees from
  Identical By State genomic data}
\usage{
calcBeeGRMIbs(x, sex, alleleFreq = NULL)

calcBeeAlleleFreq(x, sex)
}
\arguments{
\item{x}{\code{\link{matrix}} of genotypes represented as allele dosage coded
as 0, 1, or 2 in females (queens or workers) and as 0 or 1 in males
(fathers or drones); individuals are in rows and sites are in columns; no
missing values are allowed (this is not checked - you will get NAs!)}

\item{sex}{character vector denoting sex for individuals with genotypes in
\code{x} - \code{"F"} for female and \code{"M"} for male}

\item{alleleFreq}{numeric, vector of allele frequencies for the sites in
\code{x}; if \code{NULL}, then \code{\link[SIMplyBee]{calcBeeAlleleFreq}} is used}
}
\value{
matrix of genomic relatedness coefficients
}
\description{
Level 0 function that returns Genomic Relatedness Matrix (GRM)
  for honeybees from Identical By State genomic data (bi-allelic SNP
  represented as allele dosages) following the method for the sex X
  chromosome (Druet and Legarra, 2020)
}
\section{Functions}{
\itemize{
\item \code{calcBeeAlleleFreq()}: Calculate allele frequencies from honeybee genotypes

}}
\examples{
founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
SP <- SimParamBee$new(founderGenomes)
\dontshow{SP$nThreads = 1L}
SP$setTrackRec(TRUE)
SP$setTrackPed(isTrackPed = TRUE)

basePop <- createVirginQueens(founderGenomes)
drones <- createDrones(x = basePop[1], nInd = 1000)
droneGroups <- pullDroneGroupsFromDCA(drones, n = 1, nDrones = nFathersPoisson)
colony <- createColony(basePop[2])
colony <- cross(x = colony, drones = droneGroups[[1]])
colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3)

geno <- getSegSiteGeno(colony, collapse = TRUE)
sex <- getCasteSex(x = colony, collapse = TRUE)

GRM <- calcBeeGRMIbs(x = geno, sex = sex)
# You can visualise this matrix with the function image() from the package 'Matrix'

#Look at the diagonal at the relationship matrix
x <- diag(GRM)
hist(x)
summary(x)

#Look at the off-diagonal at the relationship matrix
x <- GRM[lower.tri(x = GRM, diag = FALSE)]
hist(x)
summary(x)

# Compare relationship between castes
ids <- getCasteId(colony)
idQueen <- ids$queen
idWorkers <- ids$workers
idDrones <- ids$drones

# Queen vs others
GRM[idQueen, idWorkers]
GRM[idQueen, idDrones]

# Workers vs worker
GRM[idWorkers, idWorkers]

# Workers vs drones
GRM[idWorkers, idDrones]

# Calculating allele frequencies ourselves (say, to "shift" base population)
aF <- calcBeeAlleleFreq(x = geno, sex = sex)
hist(aF)
GRM2 <- calcBeeGRMIbs(x = geno, sex = sex, alleleFreq = aF)
stopifnot(identical(GRM2, GRM))

# You can also create relationships with pooled genomes
pooledGenoW <- getPooledGeno(getWorkersSegSiteGeno(colony),
                             type = "mean",
                             sex = getCasteSex(colony, caste="workers"))
queenGeno <- getQueenSegSiteGeno(colony)
# Compute relationship between pooled workers genotype and the queen
calcBeeGRMIbs(x = rbind(queenGeno, pooledGenoW), sex = c("F","F"))
# You can now compare how this compare to relationships between the queen
# individual workers!
}
\references{
Druet and Legarra (2020) Theoretical and empirical comparisons of
  expected and realized relationships for the X-chromosome. Genetics
  Selection Evolution, 52:50 \doi{10.1186/s12711-020-00570-6}
}
