#' @useDynLib SIMplyBee, .registration = TRUE
#' @import Rcpp
#' @import AlphaSimR
#' @importFrom methods classLabel is new setClass setClassUnion setValidity
#' @importFrom methods setMethod show slot slot<- validObject
#' @importFrom R6 R6Class
#' @importFrom stats rnorm rbeta runif rpois na.omit
#' @importFrom extraDistr rtpois
#' @importFrom utils packageVersion
#' @importFrom foreach foreach "%dopar%"
#' @importFrom doParallel registerDoParallel
# see https://r-pkgs.org/namespace.html on description what to import/depend/...

#' @description
#' SIMplyBee is an extension of the AlphaSimR package
#' (https://cran.r-project.org/package=AlphaSimR) for stochastic simulations of
#' honeybee populations and breeding programmes. It enables simulation of
#' individual bees that form a colony, which includes a queen, fathers (drones
#' the queen mated with), virgin queens, workers, and drones. Multiple colony can
#' be merged into a population of colonies, such as an apiary or a whole country
#' of colonies. Functions enable operations on castes, colony, or colonies, to
#' ease R scripting of whole populations. All AlphaSimR functionality with
#' respect to genomes and genetic and phenotype values is available and further
#' extended for honeybees, including haplo-diploidy, csd locus, colony events
#' (swarming, supersedure, etc.), and colony phenotype values.
#'
#' See the introductory vignette on using this package by running:
#' \code{vignette("Honeybee_biology", package="SIMplyBee")}
#'
#' @keywords internal
"_PACKAGE"
