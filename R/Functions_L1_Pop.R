# ---- Level 1 Pop Functions  ----

#' @rdname getCastePop
#' @title Access individuals of a caste
#'
#' @description Level 1 function that returns individuals of a caste. These
#'   individuals stay in the colony (compared to \code{\link{pullCastePop}}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}},
#'   exceptionally \code{\link{Pop-class}} for calling \code{getFathers}
#'   on a queen population
#' @param caste character, "queen", "fathers", "workers", "drones",
#'   "virginQueens", or "all"
#' @param nInd numeric, number of individuals to access, if \code{NULL} all
#'   individuals are accessed; if there are less individuals than requested,
#'   we return the ones available - this can return \code{NULL}.
#'   If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies.
#' @param use character, all options provided by \code{\link{selectInd}} and
#'   \code{"order"} that selects \code{1:nInd} individuals (meaning it always
#'   returns at least one individual, even if \code{nInd = 0})
#' @param removeFathers logical, removes \code{drones} that have already mated;
#'   set to \code{FALSE} if you would like to get drones for mating with multiple
#'   virgin queens, say via insemination
#' @param collapse logical, whether to return a single merged population
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{getQueen}}, \code{\link{getFathers}},
#'   \code{\link{getVirginQueens}}, \code{\link{getWorkers}}, and
#'   \code{\link{getDrones}}
#'
#' @return when \code{x} is \code{\link{Colony-class}} return is
#'   \code{\link{Pop-class}} for \code{caste != "all"} or list for \code{caste
#'   == "all"} with nodes named by caste; when \code{x} is
#'   \code{\link{MultiColony-class}} return is a named list of
#'   \code{\link{Pop-class}} for \code{caste != "all"} or named list of lists of
#'   \code{\link{Pop-class}} for \code{caste == "all"}. You can merge
#'   all the populations in the list with \code{\link{mergePops}} function.
#'
#' @seealso \code{\link{getCasteId}} and \code{\link{getCaste}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' # Build-up and add virgin queens
#' colony <- buildUp(x = colony)
#' apiary <- buildUp(x = apiary)
#' colony <- addVirginQueens(x = colony)
#' apiary <- addVirginQueens(x = apiary)
#'
#' # Get the queen of the colony
#' getCastePop(colony, caste = "queen")
#' getQueen(colony)
#'
#' # Comparison of getCastePop() and getWorkers()
#' getCastePop(colony, caste = "workers")
#' getCastePop(colony, caste = "workers")
#' getCastePop(colony, caste = "workers", nInd = 2)
#' # Or aliases
#' getWorkers(colony)
#' # Same aliases exist for all the castes!
#'
#' # Input is a MultiColony class - same behaviour as for the Colony!
#' getCastePop(apiary, caste = "queen")
#' # Or alias
#' getQueen(apiary)
#'
#' # Sample individuals from all the castes
#' getCastePop(colony, nInd = 5, caste = "all")
#'
#' # Get different number of workers per colony
#' getCastePop(apiary, caste = "workers", nInd = c(10, 20))
#' # Or alias
#' getWorkers(apiary, nInd = c(10, 20))
#'
#' # Obtain individuals from MultiColony as a single population
#' getCastePop(apiary, caste = "queen", collapse = TRUE)
#' getQueen(apiary, collapse = TRUE)
#' getWorkers(apiary, nInd = 10, collapse = TRUE)
#' getDrones(apiary, nInd = 3, collapse = TRUE)
#' @export
getCastePop <- function(x, caste = "all", nInd = NULL, use = "rand",
                        removeFathers = TRUE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste can be only of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must be non-negative or NULL!")
  }
  if (isColony(x)) {
    if (length(nInd) > 1) {
      warning("More than one value in the nInd argument, taking only the first value!")
      nInd <- nInd[1]
    }
    if (caste == "all") {
      ret <- vector(mode = "list", length = 5)
      names(ret) <- c("queen", "fathers", "workers", "drones", "virginQueens")
      for (caste in names(ret)) {
        tmp <- getCastePop(x = x, caste = caste, nInd = nInd, use = use,
                           removeFathers = removeFathers, simParamBee = simParamBee)
        if (is.null(tmp)) {
          ret[caste] <- list(NULL)
        } else {
          ret[[caste]] <- tmp
        }
      }
      if (collapse) {
        ret <- mergePops(ret)
      }
    } else {
      if (caste == "fathers") {
        if (isQueenPresent(x, simParamBee = simParamBee)) {
          if(packageVersion("AlphaSimR") > package_version("1.5.3")){
            pop <- x@queen@misc$fathers[[1]]
          }else{
            pop <- x@queen@misc[[1]]$fathers
          }
        } else {
          pop <- NULL
        }
      } else {
        pop <- slot(x, caste)
      }
      if (is.null(pop)) {
        ret <- NULL
      } else {
        if (caste == "drones" && removeFathers) {
          test <- isDrone(pop, simParamBee = simParamBee)
          if (any(!test)) {
            pop <- pop[test]
          }
        }
        if (is.null(nInd)) {
          nInd <- nInd(pop)
        }
        nIndRequested <- nInd
        nIndAvailable <- nInd(pop)
        if (nIndRequested > nIndAvailable) {
          warning("You are requesting more individuals than available.")
          nIndRequested <- nIndAvailable
        }
        if (nIndAvailable == 0) {
          start <- 0
        } else {
          start <- 1
        }
        if (use == "order") {
          ret <- pop[start:nIndRequested]
        } else {
          ret <- selectInd(pop = pop, nInd = nIndRequested, use = use, simParam = simParamBee)
        }
      }
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nNInd <- length(nInd)
    if (nNInd > 1 && nNInd < nCol) {
      stop("Too few values in the nInd argument!")
    }
    if (nNInd > 1 && nNInd > nCol) {
      warning(paste0("Too many values in the nInd argument, taking only the first ", nCol, "values!"))
      nInd <- nInd[1:nCol]
    }
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      if (is.null(nInd)) {
        nIndColony <- NULL
      } else {
        nIndColony <- ifelse(nNInd == 1, nInd, nInd[colony])
      }
      tmp <- getCastePop(x = x[[colony]],
                         caste = caste,
                         nInd = nIndColony,
                         use = use,
                         removeFathers = removeFathers,
                         collapse = collapse,
                         simParamBee = simParamBee)
      if (is.null(tmp)) {
        ret[colony] <- list(NULL)
      } else {
        ret[[colony]] <- tmp
      }
    }
    if (collapse) {
      ret <- mergePops(ret)
    } else {
      names(ret) <- getId(x)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn getCastePop Access the queen
#' @export
getQueen <- function(x, collapse = FALSE, simParamBee = NULL) {
  ret <- getCastePop(x, caste = "queen", nInd = 1, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getCastePop Access fathers (drones the queen mated with)
#' @export
getFathers <- function(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) { # DO WE WANT TO PUT THIS IN getCastePop???
    if(packageVersion("AlphaSimR") > package_version("1.5.3")){
      ret = lapply(X = x@misc$fathers,
                   FUN = function(z){
                     if(is.null(z)){
                       ret = NULL
                     }else{
                       if (is.null(nInd)) {
                         n <- nInd(z)
                       }
                       ret <- selectInd(pop = z, nInd = n, use = use, simParam = simParamBee)
                     }
                   }
      )
    }else{
      ret <- lapply(
        X = x@misc,
        FUN = function(z) {
          if (is.null(z$fathers)) {
            ret <- NULL
          } else {
            if (is.null(nInd)) {
              n <- nInd(z$fathers)
            }
            ret <- selectInd(pop = z$fathers, nInd = n, use = use, simParam = simParamBee)
          }
        }
      )
    }
    if (nInd(x) == 1) {
      ret <- ret[[1]]
    }
  } else if (isColony(x) | isMultiColony(x)) {
    ret <- getCastePop(x, caste = "fathers", nInd = nInd, use = use, collapse = collapse, simParamBee = simParamBee)
  } else {
    stop("Argument x must be a Pop, Colony, or MultiColony class object!")
  }
  if (isMultiColony(x)) {
    if (collapse) {
      ret <- mergePops(ret)
    } else {
      names(ret) <- getId(x)
    }
  }
  return(ret)
}

#' @describeIn getCastePop Access workers
#' @export
getWorkers <- function(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL) {
  ret <- getCastePop(x, caste = "workers", nInd = nInd, use = use, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn getCastePop Access drones
#' @export
getDrones <- function(x, nInd = NULL, use = "rand", removeFathers = TRUE, collapse = FALSE, simParamBee = NULL) {
  ret <- getCastePop(x,
                     caste = "drones", nInd = nInd, use = use,
                     removeFathers = removeFathers,
                     collapse = collapse,
                     simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn getCastePop Access virgin queens
#' @export
getVirginQueens <- function(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL) {
  ret <- getCastePop(x, caste = "virginQueens", nInd = nInd, use = use,
                     collapse = collapse, simParamBee = simParamBee)
  return(ret)
}


#' @rdname createCastePop
#' @title Creates caste population individuals from the colony
#'
#' @description Level 1 function that creates the specified number of caste
#'   individuals from the colony with a mated queens. If csd
#'   locus is active, it takes it into account and any csd homozygotes are
#'   removed and counted towards homozygous brood.
#'
#' @param x \code{link{MapPop-class}} (only if caste is "virginQueens"), or
#'   \code{Pop} (only if caste is "drones") or \code{\link{Colony-class}}
#'   or \code{\link{MultiColony-class}}
#' @param caste character, "workers", "drones", or "virginQueens"
#' @param nInd numeric or function, number of caste individuals; if \code{NULL} then
#'   \code{\link{SimParamBee}$nWorkers},  \code{\link{SimParamBee}$nDrones}
#'   or \code{\link{SimParamBee}$nVirginQueens} is used depending on the caste;
#'   only used when \code{x} is \code{\link{Colony-class}} or
#'   \code{\link{MultiColony-class}}, when \code{x} is \code{link{MapPop-class}}
#'   all individuals in \code{x} are converted into virgin queens
#' @param exact logical, only relevant when creating workers,
#'   if the csd locus is active and exact is \code{TRUE},
#'   create the exactly specified number of viable workers (heterozygous on the
#'   csd locus)
#' @param year numeric, year of birth for virgin queens
#' @param editCsd logical (only active when \code{x} is \code{link{MapPop-class}}),
#'   whether the csd locus should be edited to ensure heterozygosity at the csd
#'   locus (to get viable virgin queens); see \code{csdAlleles}
#' @param csdAlleles \code{NULL} or list (only active when \code{x} is \code{link{MapPop-class}});
#'   If \code{NULL}, then the function samples a heterozygous csd genotype for
#'   each virgin queen from all possible csd alleles.
#'   If not \code{NULL}, the user provides a list of length \code{nInd} with each
#'   node holding a matrix or a data.frame, each having two rows and n columns.
#'   Each row must hold one csd haplotype (allele) that will be assigned to a
#'   virgin queen. The n columns span the length of the csd locus as specified
#'   in \code{\link{SimParamBee}}. The two csd alleles must be different to
#'   ensure heterozygosity at the csd locus.
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{nInd} when this argument is a function
#'
#' @return when \code{x} is \code{link{MapPop-class}} returns
#'   \code{virginQueens} (a \code{\link{Pop-class}});
#'   when \code{x} is \code{\link{Colony-class}} returns
#'   \code{virginQueens} (a \code{\link{Pop-class}});
#'   when \code{x} is \code{\link{MultiColony-class}}
#'   return is a named list of \code{virginQueens} (a \code{\link{Pop-class}});
#'   named by colony ID
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 50)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' SP$setTrackRec(TRUE)
#' SP$setTrackPed(isTrackPed = TRUE)
#'
#' # Create virgin queens on a MapPop
#' basePop <- createCastePop(founderGenomes, caste = "virginQueens")
#' # Or alias
#' createVirginQueens(founderGenomes)
#' # Same aliases exist for all the castes!!!
#'
#' # Create drones on a Pop
#' drones <- createDrones(x = basePop[1],  nInd = 200)
#' # Or create unequal number of drones from multiple virgin queens
#' drones <- createDrones(basePop[1:2], nInd = c(100, 200))
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' # Using default nInd in SP
#' colony@virginQueens <- createVirginQueens(colony)
#' colony@workers <- createWorkers(colony)$workers
#' colony@drones <- createDrones(colony)
#' # Usually, you would use functions buildUp() or addCastePop()
#'
#' # These populations hold individual information
#' # Example on the virgin queens (same holds for all castes!)
#' virginQueens <- colony@virginQueens
#' virginQueens@id
#' virginQueens@sex
#' virginQueens@mother
#' virginQueens@father
#'
#' # Specify own number
#' SP$nVirginQueens <- 15
#' SP$nWorkers <- 100
#' SP$nDrones <- 10
#' createVirginQueens(colony)
#' createVirginQueens(apiary)
#' # Or creating unequal numbers
#' createVirginQueens(apiary, nInd = c(5, 10))
#' # nVirginQueens will NOT vary between function calls when a constant is used
#'
#' # Specify a function that will give a number
#' createVirginQueens(colony, nInd = nVirginQueensPoisson)
#' createVirginQueens(apiary, nInd = nVirginQueensPoisson)
#' # No. of individuals will vary between function calls when a function is used
#'
#' # Store a function or a value in the SP object
#' SP$nVirginQueens <- nVirginQueensPoisson
#' createVirginQueens(colony)
#' createVirginQueens(colony)
#' createVirginQueens(apiary)
#' createVirginQueens(apiary)
#' # No. of individuals will vary between function calls when a function is used
#'
#' # csd homozygosity - relevant when creating virgin queens
#' SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes, editCsd = FALSE)
#' all(isCsdHeterozygous(basePop))
#'
#' basePop <- createVirginQueens(founderGenomes, editCsd = TRUE)
#' all(isCsdHeterozygous(basePop))
#' @export
# TODO: explore options for implementing difference between workers' and queens'
#       patrilines
#       https://github.com/HighlanderLab/SIMplyBee/issues/78
createCastePop <- function(x, caste = NULL, nInd = NULL,
                           exact = TRUE, year = NULL,
                           editCsd = TRUE, csdAlleles = NULL,
                           simParamBee = NULL,
                           ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nInd)) {
    if (caste == "virginQueens") {
      nInd <- simParamBee$nVirginQueens
    } else if (caste == "workers") {
      nInd <- simParamBee$nWorkers
    } else if (caste == "drones") {
      nInd <- simParamBee$nDrones
    }
  }
  if (is.function(nInd)) {
    nInd <- nInd(x, ...)
  }
  # doing "if (is.function(nInd))" below
  if (isMapPop(x)) {
    if (caste != "virginQueens") { # Creating virgin queens if input  is a MapPop
      stop("MapPop-class can only be used to create virgin queens!")
    }
    ret <- newPop(x, simParam = simParamBee)
    if (!is.null(simParamBee$csdChr)) {
      if (editCsd) {
        ret <- editCsdLocus(ret, alleles = csdAlleles, simParamBee = simParamBee)
      }
    }
    ret@sex[] <- "F"
    simParamBee$changeCaste(id = ret@id, caste = "virginQueens")
    if (!is.null(year)) {
      ret <- setQueensYearOfBirth(x = ret, year = year, simParamBee = simParamBee)
    }
  } else if (isPop(x)) {
    if (caste != "drones") { # Creating drones if input is a Pop
      stop("Pop-class can only be used to create drones!")
    }
    if (any(!(isVirginQueen(x, simParamBee = simParamBee) | isQueen(x, simParamBee = simParamBee)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    if (length(nInd) == 1) {
      # Diploid version - a hack, but it works
      ret <- makeDH(pop = x, nDH = nInd, keepParents = FALSE, simParam = simParamBee)
    } else {
      if (length(nInd) < nInd(x)) {
        stop("Too few values in the nInd argument!")
      }
      if (length(nInd) > 1 && length(nInd) > nInd(x)) {
        warning(paste0("Too many values in the nInd argument, taking only the first ", nInd(x), "values!"))
        nInd <- nInd[1:nInd(x)]
      }
      ret <- list()
      for (virginQueen in 1:nInd(x)) {
        ret[[virginQueen]] <- makeDH(pop = x[virginQueen], nDH = nInd[virginQueen], keepParents = FALSE, simParam = simParamBee)
      }
      ret <- mergePops(ret)
    }
    ret@sex[] <- "M"
    simParamBee$addToCaste(id = ret@id, caste = "drones")
  } else if (isColony(x)) {
    if (!isQueenPresent(x, simParamBee = simParamBee)) {
      stop("Missing queen!")
    }
    if (length(nInd) > 1) {
      warning("More than one value in the nInd argument, taking only the first value!")
      nInd <- nInd[1]
    }
    if (caste == "workers") {
      ret <- vector(mode = "list", length = 2)
      names(ret) <- c("workers", "nHomBrood")
      workers <- combineBeeGametes(
        queen = getQueen(x, simParamBee = simParamBee), drones = getFathers(x, simParamBee = simParamBee),
        nProgeny = nInd, simParamBee = simParamBee
      )
      if (isCsdActive(simParamBee = simParamBee)) {
        sel <- isCsdHeterozygous(pop = workers, simParamBee = simParamBee)
        ret$workers <- workers[sel]
        ret$nHomBrood <- nInd - sum(sel)
        if (exact) {
          if (nInd(ret$workers) < nInd) {
            nMiss <- nInd - nInd(ret$workers)
            while (0 < nMiss) {
              workers <- combineBeeGametes(
                queen = getQueen(x, simParamBee = simParamBee),
                drones = getFathers(x, simParamBee = simParamBee),
                nProgeny = nMiss,
                simParamBee = simParamBee
              )
              sel <- isCsdHeterozygous(pop = workers, simParamBee = simParamBee)
              ret$workers <- c(ret$workers, workers[sel])
              ret$nHomBrood <- ret$nHomBrood + sum(!sel)
              nMiss <- nInd - nInd(ret$workers)
            }
          }
        }
      } else {
        ret$workers <- workers
        ret$nHomBrood <- NA
      }
      ret$workers@sex[] <- "F"
      simParamBee$addToCaste(id = ret$workers@id, caste = "workers")
    } else if (caste == "virginQueens") { # Creating virgin queens if input is a Colony
      ret <- createCastePop(x = x, caste = "workers",
                            nInd = nInd, exact = TRUE, simParamBee = simParamBee)$workers
      ret@sex[] <- "F"
      simParamBee$changeCaste(id = ret@id, caste = "virginQueens")
      if (!is.null(year)) {
        ret <- setQueensYearOfBirth(x = ret, year = year, simParamBee = simParamBee)
      }
    } else if (caste == "drones") { # Creating drones if input is a Colony
      ret <- makeDH(
        pop = getQueen(x, simParamBee = simParamBee), nDH = nInd, keepParents = FALSE,
        simParam = simParamBee
      )
      ret@sex[] <- "M"
      simParamBee$addToCaste(id = ret@id, caste = "drones")
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nNInd <- length(nInd)
    if (nNInd > 1 && nNInd < nCol) {
      stop("Too few values in the nInd argument!")
    }
    if (nNInd > 1 && nNInd > nCol) {
      warning(paste0("Too many values in the nInd argument, taking only the first ", nCol, "values!"))
      nInd <- nInd[1:nCol]
    }
    ret <- vector(mode = "list", length = nCol)
    for (colony in seq_len(nCol)) {
      if (is.null(nInd)) {
        nIndColony <- NULL
      } else {
        nIndColony <- ifelse(nNInd == 1, nInd, nInd[colony])
      }
      ret[[colony]] <- createCastePop(
        x = x[[colony]], caste = caste,
        nInd = nIndColony,
        exact = exact,
        year = year,
        editCsd = TRUE, csdAlleles = NULL,
        simParamBee = simParamBee, ...
      )
    }
    names(ret) <- getId(x)
  } else {
    stop("Argument x must be a Map-Pop (only for virgin queens),
         Pop (only for drones), Colony, or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn createCastePop Create workers from a colony
#' @export
createWorkers <- function(x, nInd = NULL, exact = FALSE, simParamBee = NULL, ...) {
  ret <- createCastePop(x, caste = "workers", nInd = nInd,
                        exact = exact, simParamBee = simParamBee, ...)
  return(ret)
}

#' @describeIn createCastePop Create drones from a colony
#' @export
createDrones <- function(x, nInd = NULL, simParamBee = NULL, ...) {
  ret <- createCastePop(x, caste = "drones", nInd = nInd,
                        simParamBee = simParamBee, ...)
  return(ret)
}

#' @describeIn createCastePop Create virgin queens from a colony
#' @export
createVirginQueens <- function(x, nInd = NULL,
                               year = NULL,
                               editCsd = TRUE, csdAlleles = NULL,
                               simParamBee = NULL,
                               ...) {
  ret <- createCastePop(x, caste = "virginQueens", nInd = nInd,
                        year = year, editCsd = editCsd,
                        csdAlleles = csdAlleles, simParamBee = simParamBee, ...)
  return(ret)
}

#' @rdname combineBeeGametes
#' @title Create diploid gametes from a mated queen
#'
#' @description Level 1 function that produces diploid offspring from a mated queen.
#'   Queen is diploid, while drones are double haploids so we use AlphaSimR diploid
#'   functionality to make this cross, but since drones are double haploids we
#'   get the desired outcome. This is an utility function, and you most likely
#'   want to use the \code{\link{cross}} functions.
#'
#' @param queen \code{\link{Pop-class}}, with a single diploid individual
#' @param drones \code{\link{Pop-class}}, with one or more diploid (double
#'   haploid) individual(s)
#' @param nProgeny integer, number of progeny to create per cross
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Pop-class}} with diploid individuals
#'
#' # Not exporting this function, since its just a helper
combineBeeGametes <- function(queen, drones, nProgeny = 1, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (nInd(queen) > 1) {
    stop("At the moment we only cater for crosses with a single queen!")
  }
  ret <- randCross2(
    females = queen, males = drones,
    nCrosses = nProgeny, nProgeny = 1, balance = FALSE,
    simParam = simParamBee
  )
  return(ret)
}

#' @rdname combineBeeGametesHaploDiploid
#' @title Create diploid gametes from a mated queen
#'
#' @description Level 1 function that produces diploid offspring from a mated queen.
#'   Drones are haploid, while the queen is diploid, so we first generate gametes
#'   (with recombination) from her and merge them with drone genomes (=gametes),
#'   where we randomly re-sample drones to get the desired number of progeny.
#'   This is an utility function, and you most likely want to use the
#'   \code{\link{cross}} function.
#'
#' @param queen \code{\link{Pop-class}}, with a single diploid individual
#' @param drones \code{\link{Pop-class}}, with one or more haploid individual(s)
#' @param nProgeny integer, number of progeny to create per cross
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details This would be the right approach to handle haplo-diploid inheritance
#'   in bees, but it causes a raft of downstream issues, since AlphaSimR assumes
#'   that individuals have the same ploidy. Hence, we don't use this function.
#'
#' @return \code{\link{Pop-class}} with diploid individuals
#'
combineBeeGametesHaploDiploid <- function(queen, drones, nProgeny = 1, simParamBee = NULL) {
  # An attempt to have drones properly haploid, but have hit AlphaSimR limits
  #   since a lot of the underlying C++ code assumes the same ploidy for all/most
  #   individuals, particularly for IBD tracking. Keeping the function here for
  #   future, but not exporting it.
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (nInd(queen) > 1) {
    stop("At the moment we only cater for crosses with a single queen!")
  }

  # Closely following AlphaSimR:::reduceGenome() and AlphaSimR:::mergeGenome()
  crossPlan <- cbind(
    match(x = queen@id, table = queen@id), # this handles more than one queen!
    match(
      x = sample(x = drones@id, size = nProgeny, replace = TRUE),
      table = drones@id
    )
  )

  # Meiosis on the queen's side
  createReducedGenome <- utils::getFromNamespace(
    x = "createReducedGenome",
    ns = "AlphaSimR"
  )
  queenGametes <- createReducedGenome(
    queen@geno, # this handles more than one queen!
    nProgeny,
    simParamBee$femaleMap,
    simParamBee$v,
    simParamBee$p,
    simParamBee$isTrackRec,
    queen@ploidy,
    simParamBee$femaleCentromere,
    simParamBee$quadProb,
    simParamBee$nThreads
  )
  dim(queenGametes$geno) <- NULL

  # Merge queen's gametes and drones (drones are haploid anyway)
  geno <- vector(mode = "list", length = simParamBee$nChr)
  for (chr in 1:simParamBee$nChr) {
    geno[[chr]] <- array(
      data = as.raw(0),
      dim = c(dim(queen@geno[[chr]])[1], 2, nProgeny)
    )
    for (prog in 1:nProgeny) {
      geno[[chr]][, 1, prog] <- queenGametes$geno[[chr]][, , crossPlan[prog, 1]] # this handles more than one queen!
      geno[[chr]][, 2, prog] <- drones@geno[[chr]][, , crossPlan[prog, 2]]
    }
  }

  rPop <- new(
    Class = "RawPop",
    nInd = as.integer(nProgeny),
    nChr = simParamBee$nChr,
    ploidy = 2L,
    nLoci = queen@nLoci,
    geno = geno
  )

  if (simParamBee$isTrackRec) {
    # Create history for haplotypes
    hist <- vector(mode = "list", length = 2L)
    hist[[1]] <- cbind(1L, 1L) # queen's contribution (will be rewritten below)
    hist[[2]] <- cbind(1L, 1L) # drones' contribution (just one chrom and no recomb)
    hist <- rep(list(hist), times = rPop@nChr) # replicate for chromosomes
    hist <- rep(list(hist), times = rPop@nInd) # replicate for individuals
    # Add queen's gamete recombinations
    for (prog in 1:nProgeny) {
      for (chr in 1:simParamBee$nChr) {
        hist[[prog]][[chr]][[1L]] <- queenGametes$recHist[[prog]][[chr]][[1L]]
      }
    }
  } else {
    hist <- NULL
  }

  return(newPop(
    rawPop = rPop,
    mother = queen@id[crossPlan[, 1]],
    father = drones@id[crossPlan[, 2]],
    simParam = simParamBee,
    iMother = queen@iid[crossPlan[, 1]],
    iFather = drones@iid[crossPlan[, 2]],
    femaleParentPop = queen,
    maleParentPop = drones,
    hist = hist
  ))
}

#' @rdname createDCA
#' @title Create a drone congregation area (DCA)
#'
#' @description Level 1 function that creates a population of drones from a Colony
#'   or MultiColony.  Such a population is often referred to as a drone
#'   congregation area (DCA).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param nInd numeric, number of random drones to pull from each colony,
#'   if \code{NULL} all drones in a colony are pulled
#' @param removeFathers logical, removes \code{drones} that have already mated;
#'   set to \code{FALSE} if you would like to get drones for mating with multiple
#'   virgin queens, say via insemination
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details In reality, drones leave the colony to mate. They die after that.
#'   In this function we only get a copy of drones from \code{x}, for
#'   computational efficiency and ease of use. However, any mating will change
#'   the caste of drones to fathers, and they won't be available for future
#'   matings (see \code{\link{cross}}). Not unless
#'   \code{removeFathers = FALSE}.
#'
#' @return \code{\link{Pop-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' colony <- addDrones(colony, nInd = 10)
#' createDCA(colony)
#' createDCA(colony, nInd = 10)@id
#'
#' apiary <- addDrones(apiary)
#' createDCA(apiary)
#' createDCA(apiary, nInd = 10)
#' @export
createDCA <- function(x, nInd = NULL, removeFathers = TRUE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isColony(x)) {
    DCA <- getDrones(x, nInd = nInd, removeFathers = removeFathers, simParamBee = simParamBee)
    if (is.null(DCA)) {
      warning("No available drones!")
    }
  } else if (isMultiColony(x)) {
    DCA <- getDrones(x, nInd = nInd, removeFathers = removeFathers, simParamBee = simParamBee)
    DCA <- DCA[sapply(DCA, FUN = function(z) !is.null(z))]
    if (length(DCA) != 0) {
      DCA <- mergePops(popList = DCA)
    } else {
      warning("No available drones!")
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(DCA)
}

#' @rdname createMatingStationDCA
#' @title Create a DCA of drones at a mating stations
#'
#' @description Level 1 function that creates a DCA at a classical honeybee
#'   mating station of several sister drone producing queens. The
#'   functions first creates multiple drone producing queens (DPQs) from one colony;
#'   and second, produces drones from the DPQs. All the created drones form a
#'   DCA at a mating station.
#'
#' @param colony \code{\link{Colony-class}} to produce drone producing queens from
#' @param nDPQs integer, the number of drone producing queens
#' @param nDronePerDPQ integer, number of drones each DPQ contributed to the DCA
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Pop-class}} with created drones resembling a DCA at a mating station
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#' drones <- createDrones(basePop[1], n = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#'
#' # Create a colony and cross it
#' colony1 <- createColony(x = basePop[2])
#' colony1 <- cross(colony1, drones = droneGroups[[1]])
#'
#' # Create a empty colony
#' colony2 <- createColony(x = basePop[3])
#'
#' # Create a mating station from colony1
#' matingStation <- createMatingStationDCA(colony1, nDPQs = 20, nDronePerDPQ = 10)
#'
#' # Cross colony2 on the mating station
#' fathers <- pullDroneGroupsFromDCA(matingStation, n = 1, nDrones = 15)
#' colony2 <- cross(colony2, drones = fathers[[1]])
#' nFathers(colony2)
#'
#' @export
createMatingStationDCA <- function(colony, nDPQs = 20, nDronePerDPQ = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isColony(colony)) {
    stop("The argument colony must be a Colony class!")
  }
  if (is.null(nDronePerDPQ)) {
    nDronePerDPQ <- simParamBee$nDrones
  }
  if (is.function(nDronePerDPQ)) {
    nDronePerDPQ <- nDronePerDPQ(n = nDPQs)
  }
  DPQs <- createVirginQueens(colony, nInd = nDPQs, simParamBee = simParamBee)
  drones <- createDrones(DPQs, nInd = nDronePerDPQ, simParamBee = simParamBee)
  return(drones)
}

#' @rdname pullInd
#' @title Pull individuals from a population
#'
#' @description Level 1 function that pulls individuals from a population and
#'   update the population (these individuals don't stay in a population).
#'
#' @param pop \code{\link{Pop-class}}
#' @param nInd numeric, number of individuals to pull, if \code{NULL} pull all
#'   individuals
#' @param use character, all options provided by \code{\link{selectInd}}
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with a node \code{pulled} holding \code{\link{Pop-class}} of
#'   pulled individuals and a node \code{remnant)} holding \code{\link{Pop-class}}
#'   of remaining individuals
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParam$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- newPop(founderGenomes)
#'
#' pullInd(basePop, nInd = 2)
#' pullInd(basePop, nInd = 3)
#' pullInd(basePop)
#' @export
pullInd <- function(pop, nInd = NULL, use = "rand", simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(pop)) {
    stop("Argument pop must be a Pop class object!")
  }
  if (is.null(nInd)) {
    nInd <- nInd(pop)
  }
  pulled <- selectInd(pop = pop, nInd = nInd, use = use, simParam = simParamBee)
  sel <- pop@id %in% pulled@id
  remnant <- pop[!sel]
  ret <- list(pulled = pulled, remnant = remnant)
  return(ret)
}

#' @rdname pullDroneGroupsFromDCA
#' @title Pulls drone groups from a Drone Congregation Area (DCA)
#'
#' @description Level 1 function that pulls drone groups from a Drone
#'   Congregation Area (DCA) to use them later in mating. Within the function
#'   drones are pulled (removed) from the DCA to reflect the fact that drones
#'   die after mating, so they can't be present in the DCA anymore. Be careful
#'   what you do with the DCA object outside function to avoid drone "copies".
#'
#' @param DCA \code{\link{Pop-class}}, population of drones
#' @param n integer, number of drone groups to be created
#' @param nDrones numeric of function, number of drones that a virgin queen
#'    mates with; if \code{NULL} then \code{\link{SimParamBee}$nFathers} is used
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... additional arguments passed to \code{nDrones} when this argument is a function
#'
#' @return list of \code{\link{Pop-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- addDrones(colony, nInd = 100)
#'
#' # Create colony DCA
#' DCA <- createDCA(colony)
#' pullDroneGroupsFromDCA(DCA, n = 4, nDrones = 5)
#' pullDroneGroupsFromDCA(DCA, n = 5, nDrones = nFathersPoisson)
#'
#' @export
pullDroneGroupsFromDCA <- function(DCA, n, nDrones = NULL,
                                   simParamBee = NULL, ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!isPop(DCA)) {
    stop("Argument DCA must be a Pop class object!")
  }
  # Keep only the drones (remove the fathers)
  DCA <- DCA[isDrone(DCA, simParamBee = simParamBee)]
  if (any(!isDrone(DCA, simParamBee = simParamBee))) {
    stop("Individuals in DCA must be drones!")
  }
  if (is.null(nDrones)) {
    nDrones <- simParamBee$nFathers
  }
  # doing "if (is.function(nDrones))" below
  ret <- vector(mode = "list", length = n)
  for (group in seq_len(n)) {
    if (is.function(nDrones)) {
      nD <- nDrones(...) # see nFathersPoisson
    } else {
      nD <- nDrones
    }
    if (nInd(DCA) < nD) {
      stop("We ran out of drones in the DCA!")
    }
    tmp <- pullInd(pop = DCA, nInd = nD, simParamBee = simParamBee)
    ret[[group]] <- tmp$pulled
    DCA <- tmp$remnant
  }
  return(ret)
}

#' @rdname pullCastePop
#' @title Pull individuals from a caste in a colony
#'
#' @description Level 1 function that pulls individuals from a caste in a
#'   colony. These individuals are removed from the colony (compared to
#'   \code{\link{getCaste}}).
#'
#' @param x \code{\link{Colony-class}} or \code{\link{MultiColony-class}}
#' @param caste character, "queen", "workers", "drones", or "virginQueens"
#' @param nInd numeric, number of individuals to pull, if \code{NULL} all
#'   individuals are pulled. If input is \code{\link{MultiColony-class}},
#'   the input could also be a vector of the same length as the number of colonies. If
#'   a single value is provided, the same value will be applied to all the colonies.
#' @param use character, all options provided by \code{\link{selectInd}}
#' @param removeFathers logical, removes \code{drones} that have already mated;
#'   set to \code{FALSE} if you would like to get drones for mating with multiple
#'   virgin queens, say via insemination
#' @param collapse logical, whether to return a single merged population
#'   for the pulled individuals (does not affect the remnant colonies)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @seealso \code{\link{pullQueen}}, \code{\link{pullVirginQueens}},
#'   \code{\link{pullWorkers}}, and \code{\link{pullDrones}}
#'
#' @return list of \code{\link{Pop-class}} and \code{\link{Colony-class}}
#'   when \code{x} is \code{\link{Colony-class}} and list of (a list of
#'   \code{\link{Pop-class}} named by colony id) and
#'   \code{\link{MultiColony-class}} when \code{x} is
#'   \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(colony, drones = droneGroups[[1]])
#' colony <- buildUp(x = colony, nWorkers = 100, nDrones = 10, exact = TRUE)
#' colony <- addVirginQueens(x = colony, nInd = 3)
#'
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#' apiary <- buildUp(x = apiary, nWorkers = 100, nDrones = 10, exact = TRUE)
#' apiary <- addVirginQueens(x = apiary, nInd = 3)
#'
#' # pullCastePop on Colony class
#' # We can't pull the queen and leave the colony queenless
#' pullCastePop(colony, caste = "virginQueens")
#' pullCastePop(colony, caste = "virginQueens", nInd = 2)
#' # Or use aliases
#' pullVirginQueens(colony)
#' pullVirginQueens(colony, nInd = 2)
#' # Same aliases exist for all the castes!!!
#'
#' # pullCastePop on MultiColony class - same behaviour as for the Colony!
#' pullCastePop(apiary, caste = "workers")
#' # Or pull out unequal number of workers from colonies
#' pullCastePop(apiary, caste = "workers", nInd = c(10, 20))
#' pullWorkers(apiary)
#' nWorkers(apiary)
#' nWorkers(pullWorkers(apiary)$remnant)
#'
#'
#' # Merge all the pulled populations into a single population
#' pullCastePop(apiary, caste = "queen", collapse = TRUE)
#' pullCastePop(apiary, caste = "virginQueens", collapse = TRUE)
#' @export
pullCastePop <- function(x, caste, nInd = NULL, use = "rand",
                         removeFathers = TRUE, collapse = FALSE, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (length(caste) > 1) {
    stop("Argument caste can be only of length 1!")
  }
  if (any(nInd < 0)) {
    stop("nInd must be non-negative or NULL!")
  }
  if (isColony(x)) {
    if (length(nInd) > 1) {
      warning("More than one value in the nInd argument, taking only the first value!")
      nInd <- nInd[1]
    }
    if (is.null(slot(x, caste))) {
      ret <- list(pulled = NULL, remnant = x)
    } else {
      if (is.null(nInd)) {
        nInd <- nInd(slot(x, caste))
      }
      tmp <- pullInd(pop = slot(x, caste), nInd = nInd, use = use, simParamBee = simParamBee)
      if (caste == "queen") {
        slot(x, caste) <- NULL
      } else {
        slot(x, caste) <- tmp$remnant
      }
      if (caste == "drones" && removeFathers) {
        test <- isDrone(tmp$pulled, simParamBee = simParamBee)
        if (any(!test)) {
          tmp$pulled <- tmp$pulled[test]
        }
      }
      ret <- list(pulled = tmp$pulled, remnant = x)
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    nNInd <- length(nInd)
    if (nNInd > 1 && nNInd < nCol) {
      stop("Too few values in the nInd argument!")
    }
    if (nNInd > 1 && nNInd > nCol) {
      warning(paste0("Too many values in the nInd argument, taking only the first ", nCol, "values!"))
      nInd <- nInd[1:nCol]
    }
    ret <- vector(mode = "list", length = 2)
    names(ret) <- c("pulled", "remnant")
    ret$pulled <- vector(mode = "list", length = nCol)
    names(ret$pulled) <- getId(x)
    ret$remnant <- x
    for (colony in seq_len(nCol)) {
      if (is.null(nInd)) {
        nIndColony <- NULL
      } else {
        nIndColony <- ifelse(nNInd == 1, nInd, nInd[colony])
      }
      tmp <- pullCastePop(x = x[[colony]],
                          caste = caste,
                          nInd = nIndColony,
                          use = use,
                          removeFathers = removeFathers,
                          collapse = collapse,
                          simParamBee = simParamBee)
      if (!is.null(tmp$pulled)) {
        ret$pulled[[colony]] <- tmp$pulled
      }
      ret$remnant[[colony]] <- tmp$remnant
    }
    if (collapse) {
      ret$pulled <- mergePops(ret$pulled)
    }
  } else {
    stop("Argument x must be a Colony or MultiColony class object!")
  }
  return(ret)
}

#' @describeIn pullCastePop Pull queen from a colony
#' @export
pullQueen <- function(x, collapse = FALSE, simParamBee = NULL) {
  ret <- pullCastePop(x, caste = "queen", collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn pullCastePop Pull workers from a colony
#' @export
pullWorkers <- function(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL) {
  ret <- pullCastePop(x, caste = "workers", nInd = nInd, use = use, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @describeIn pullCastePop Pull drones from a colony
#' @export
pullDrones <- function(x, nInd = NULL, use = "rand", removeFathers = TRUE, collapse = FALSE, simParamBee = NULL) {
  ret <- pullCastePop(x,
                      caste = "drones", nInd = nInd, use = use,
                      removeFathers = removeFathers,
                      collapse = collapse,
                      simParamBee = simParamBee
  )
  return(ret)
}

#' @describeIn pullCastePop Pull virgin queens from a colony
#' @export
pullVirginQueens <- function(x, nInd = NULL, use = "rand", collapse = FALSE, simParamBee = NULL) {
  ret <- pullCastePop(x, caste = "virginQueens", nInd = nInd, use = use, collapse = collapse, simParamBee = simParamBee)
  return(ret)
}

#' @rdname cross
#' @title Cross (mate) virgin queen(s) as a population, of a colony, or
#'   of all given colonies
#'
#' @description Level 1 function that crosses (mates) a virgin queen to a group
#'   of drones. The virgin queen(s) could be within a population (\code{\link{Pop-class}}),
#'   in a colony (\code{\link{Colony-class}}), or multi-colony (\code{\link{MultiColony-class}}).
#'   This function does not create any progeny, it only stores the mated drones
#'   (fathers) so we can later create progeny as needed. When input is a
#'   (\code{\link{Colony-class}}) or (\code{\link{MultiColony-class}}), one
#'   virgin queens is selected at random, mated, and promoted to the queen of
#'   the colony. Other virgin queens are destroyed. Mated drones (fathers) are
#'   stored for producing progeny at a later stage. For a better understanding
#'   of crossing and the functions have a look at the "Crossing" vignette.
#'
#' @param x \code{\link{Pop-class}} or code{\link{Colony-class}} or \code{\link{MultiColony-class}},
#'   one or more virgin queens / colonies to be mated;
#' @param crossPlan, named list with names being virgin queen or colony IDs with
#'   each list element holding the IDs of either selected drones or selected
#'   drone producing colonies, OR a string "create" if you want the cross plan to be created
#'   internally. The function can create a random (\code{spatial = FALSE})
#'   or spatial (\code{spatial = TRUE}) cross plan internally. Also see
#'   \code{\link{createCrossPlan}}.
#' @param drones a \code{\link{Pop-class}} or a list of \code{\link{Pop-class}},
#'   group(s) of drones that will be mated with virgin queen(s).
#'   See \code{\link{pullDroneGroupsFromDCA}}) to create a list of drone "packages".
#'   A single \code{\link{Pop-class}} is only allowed when mating
#'   a single virgin queen or colony, or when mating according to a cross plan that
#'   includes drones' IDs. When creating a spatial cross plan internally, males
#'   can not be provided through the \code{drones} argument as a \code{\link{Pop-class}},
#'   but should be provided through the \code{droneColonies} argument as
#'   or \code{\link{MultiColony-class}}
#' @param droneColonies \code{\link{MultiColony-class}} with all available
#'   drone producing colonies. Provided when \code{drones} is not provided (NULL). When
#'   providing drone producing colonies, the cross function uses a cross plan, that can
#'   either be provided by the user through (\code{crossPlan}) argument or created
#'   internally (when \code{crossPlan} is "create")
#' @param nDrones numeric of function, the number of drones to sample to mate with each
#'   virgin queen when using a cross plan
#' @param spatial logical, whether the drone producing colonies should be sampled according
#'   to their distance from the virgin colony (that is, in a radius)
#' @param radius numeric, the radius around the virgin colony in which to sample mating partners,
#'   only needed when \code{spatial = TRUE}
#' @param checkCross character, throw a warning (when \code{checkCross = "warning"}),
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#' @param ... other arguments for \code{nDrones}, when \code{nDrones} is a function
#'
#' @details This function changes caste for the mated drones to fathers, and
#'   mated virgin queens to queens. See examples. This means that you can not
#'   use these individuals in matings any more!
#'
#'   If the supplied drone population is empty (has 0 individuals), which
#'   can happen in edge cases or when \code{\link{nFathersPoisson}} is used
#'   instead of \code{\link{nFathersTruncPoisson}}, or when performing spatially-aware mating
#'   and no drone producing colonies are found in the vicinity, then mating of a virgin
#'   queen will fail and she will stay virgin. This can happen for just a few
#'   of many virgin queens, which can be annoying to track down, but you can use
#'   \code{\link{isQueen}} or \code{\link{isVirginQueen}} to find such virgin
#'   queens. You can use \code{checkCross} to alert you about this situation.
#'
#' @seealso \code{\link{Colony-class}} on how we store the fathers along the
#'   queen.  For more examples for mating with either externally or internally created cross plan,
#'  please see \code{\link{createCrossPlan}}
#'
#' @return \code{\link{Pop-class}} with mated queen(s). The misc slot of the
#'   queens contains additional information about the number of workers, drones,
#'   and homozygous brood produced, and the expected percentage of csd homozygous
#'   brood.
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 30, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 20, nDrones = nFathersPoisson)
#'
#' # If input is a Pop class of virgin queen(s)
#' virginQueen <- basePop[2]
#' isQueen(virginQueen)
#' (matedQueen <- cross(
#'   x = virginQueen,
#'   drones = droneGroups[1]
#' ))
#'
#' isQueen(virginQueen)
#' isQueen(matedQueen)
#' nFathers(matedQueen)
#' isDrone(getFathers(matedQueen))
#' isFather(getFathers(matedQueen))
#'
#' virginQueens <- basePop[4:5]
#' matedQueens <- cross(
#'   x = virginQueens,
#'   drones = droneGroups[c(3, 4)]
#' )
#'
#' isQueen(matedQueens)
#' nFathers(matedQueens)
#' getFathers(matedQueens)
#'
#' # Inbred mated queen (mated with her own sons)
#' matedQueen2 <- cross(
#'   x = basePop[1],
#'   drones = droneGroups[5]
#' )
#' # Check the expected csd homozygosity
#' pHomBrood(matedQueen2)
#'
#' # If input is a Colony or MultiColony class
#' # Create Colony and MultiColony class
#' colony <- createColony(basePop[6])
#' isVirginQueen(getVirginQueens(colony))
#' apiary <- createMultiColony(basePop[7:8])
#' all(isVirginQueen(mergePops(getVirginQueens(apiary))))
#'
#' # Cross
#' colony <- cross(colony, drones = droneGroups[[6]])
#' isQueenPresent(colony)
#' apiary <- cross(apiary, drones = droneGroups[c(7, 8)])
#' all(isQueenPresent(apiary))
#' nFathers(apiary)
#'
#' # Try mating with drones that were already used for mating
#' colony <- createColony(basePop[9])
#' try((matedColony <- cross(x = colony, drones = droneGroups[[1]])))
#' # Create new drones and mate the colony with them
#' drones <- createDrones(x = basePop[1], nInd = 15)
#' all(isDrone(drones))
#' any(isFather(drones))
#' (matedColony <- cross(x = colony, drones = drones))
#' isQueenPresent(matedColony)
#'
#' # Mate with drone producing colonies and a given cross plan
#' droneColonies <- createMultiColony(basePop[10:15])
#' droneColonies <- cross(droneColonies, drones = droneGroups[10:15])
#' nFathers(droneColonies)
#' apiary2 <- createMultiColony(basePop[16:20])
#' apiary3 <- createMultiColony(basePop[21:25])
#' apiary4 <- createMultiColony(basePop[26:30])
#'
#' # Create a random cross plan
#' randomCrossPlan <- createCrossPlan(x = apiary2,
#'                                    droneColonies = droneColonies,
#'                                    nDrones = nFathersPoisson,
#'                                    spatial = FALSE)
#' apiary2 <- cross(x = apiary2,
#'                  droneColonies = droneColonies,
#'                  crossPlan = randomCrossPlan,
#'                  nDrones = 15)
#' nFathers(apiary2)
#'
#' # Mate colonies according to a cross plan that is created internally within the cross function
#' apiary3 <- cross(x = apiary3,
#'                  droneColonies = droneColonies,
#'                  crossPlan = "create",
#'                  nDrones = 15)
#' nFathers(apiary3)
#'
#' # Mate colonies according to a cross plan that is created internally within the cross function
#' # For this, all the colonies have to have a set location
#' droneColonies <- setLocation(droneColonies,
#'                              location = Map(c, runif(6, 0, 2*pi), runif(6, 0, 2*pi)))
#' apiary4 <- setLocation(apiary4,
#'                        location = Map(c, runif(5, 0, 2*pi), runif(5, 0, 2*pi)))
#'
#' apiary4 <- cross(x = apiary4,
#'                  droneColonies = droneColonies,
#'                  crossPlan = "create",
#'                  nDrones = nFathersPoisson,
#'                  spatial = TRUE,
#'                  checkCross = "warning",
#'                  radius = 3)
#' nFathers(apiary4)
#'
#' @seealso For crossing virgin queens according to a cross plan, see
#'   \code{\link{createCrossPlan}}.
#'   For crossing virgin queens on a mating stations, see
#'   \code{\link{createMatingStationDCA}}
#'
#' @export
cross <- function(x,
                  crossPlan = NULL,
                  drones = NULL,
                  droneColonies = NULL,
                  nDrones = NULL,
                  spatial = FALSE,
                  radius = NULL,
                  checkCross = "error",
                  simParamBee = NULL,
                  ...) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(nDrones)) {
    nDrones <- simParamBee$nFathers
  }
  if (is.function(nDrones)) {
    nD <- nDrones(...)
  } else {
    nD <- nDrones
  }

  IDs <- as.character(getId(x))
  oneColony <- (isPop(drones)) && (length(IDs) == 1) && (is.null(crossPlan))
  dronePackages <- is.list(drones)
  crossPlan_given <- !dronePackages && is.list(crossPlan)
  crossPlan_create <-  ifelse(!is.null(crossPlan) && !dronePackages, (crossPlan[1] == "create"), FALSE)
  crossPlan_droneID <- (!is.null(crossPlan)) && !is.null(drones)
  crossPlan_colonyID <- (!is.null(crossPlan)) && !is.null(droneColonies)


  # Do all the tests here to simplify the function
  if (crossPlan_droneID && !isPop(drones)) {
    stop("When using a cross plan, drones must be supplied as a single Pop-class!")
  }
  if (crossPlan_colonyID && !isMultiColony(droneColonies)) {
    stop("When using a cross plan, droneColonies must be supplied as a single MultiColony-class!")
  }
  if (!is.null(drones) && !is.null(droneColonies)) {
    stop("You can provide either drones or droneColonies, but not both!")
  }
  if (is.null(drones) & is.null(droneColonies)) {
    stop("You must provide either drones or droneColonies!")
  }
  if (!dronePackages & !isPop(drones) & is.null(droneColonies)) {
    stop("The argument drones must be a Pop-class
         or a list of drone Pop-class objects!")
  }
  if (crossPlan_given && !is.null(drones) && !all(unlist(crossPlan) %in% drones@id)) {
    stop("Some drones from the crossPlan are missing in the drones population!")
  }
  if (dronePackages && length(IDs) != length(drones)) { #check for list of father pops
    stop("Length of argument drones should match the number of virgin queens/colonies!")
  }
  if (!is.null(crossPlan) && all(is.null(drones), is.null(droneColonies))) {
    stop("When providing a cross plan, you must also provide drones or droneColonies!")
  }
  if (crossPlan_given && !all(IDs %in% names(crossPlan))) { #Check for cross plan
    stop("Cross plan must include all the virgin queens/colonies!")
  }
  if (isPop(x)) {
    if (any(!isVirginQueen(x, simParamBee = simParamBee))) {
      stop("Individuals in pop must be virgin queens!")
    }
  }
  if (isColony(x) | isMultiColony(x)) {
    if (any(isQueenPresent(x, simParamBee = simParamBee))) {
      stop("Queen already present in the colony!")
    }
    if (any(!isVirginQueensPresent(x, simParamBee = simParamBee))) {
      stop("No virgin queen(s) in the colony to cross!")
    }
  }


  if (crossPlan_create) {
    crossPlan <- createCrossPlan(x = x,
                                 drones = drones,
                                 droneColonies = droneColonies,
                                 nDrones = nDrones,
                                 spatial = spatial,
                                 radius = radius,
                                 simParamBee = simParamBee)
    noMatches <- sapply(crossPlan, FUN = length)
    if (0 %in% noMatches) {
      message("There are no potential crosses for some colonies! The cross() will fail
              unless argument checkCross is set to 'warning'.")
    }
  }
  if (isPop(x) | isColony(x)) {
    ret <- list()
    for (virgin in seq_len(length(IDs))) {
      virginID <- IDs[virgin]
      if (oneColony) {
        virginQueenDrones <- drones
      } else if (dronePackages) {
        virginQueenDrones <- drones[[virgin]]
      } else if (crossPlan_given | crossPlan_create) {
        if (crossPlan_droneID) {
          virginQueenDrones <- drones[crossPlan[[virginID]]]
        } else if (crossPlan_colonyID) {
          virginMatches <- crossPlan[[virginID]]
          if (length(virginMatches) > 0) {
            nD <- ifelse(is.function(nDrones), nDrones(...), nDrones)
            selectedDPQ <- table(sample(virginMatches, size = nD, replace = TRUE))
            virginQueenDrones <- mergePops(createDrones(droneColonies[names(selectedDPQ)],
                                                        nInd = selectedDPQ, simParamBee = simParamBee))
          } else {
            virginQueenDrones <- new("Pop")
          }
        }
      }

      if (any((virginQueenDrones@nInd == 0), (length(virginQueenDrones@nInd) == 0))) {
        msg <- "Crossing failed!"
        if (checkCross == "warning") {
          message(msg)
          ret <- x
        } else if (checkCross == "error") {
          stop(msg)
        }
      } else if (virginQueenDrones@nInd > 0) {
        if (!all(isDrone(virginQueenDrones, simParamBee = simParamBee))) {
          stop("Individuals in drones must be drones!")
        }
        if (isPop(x)) {
          virginQueen <- x[virgin]
        } else if (isColony(x)) {
          virginQueen <- selectInd(x@virginQueens, nInd = 1, use = "rand", simParam = simParamBee)
        }

        if(packageVersion("AlphaSimR") > package_version("1.5.3")){
          virginQueen@misc$fathers[[1]] <- virginQueenDrones
        }else{
          virginQueen@misc[[1]]$fathers <- virginQueenDrones
        }
        simParamBee$changeCaste(id = virginQueen@id, caste = "queen")
        simParamBee$changeCaste(id = virginQueenDrones@id, caste = "fathers")

        virginQueen <- setMisc(x = virginQueen, node = "nWorkers", value = 0)
        virginQueen <- setMisc(x = virginQueen, node = "nDrones", value = 0)
        virginQueen <- setMisc(x = virginQueen, node = "nHomBrood", value = 0)
        if (isCsdActive(simParamBee = simParamBee)) {
          val <- calcQueensPHomBrood(x = virginQueen, simParamBee = simParamBee)
        } else {
          val <- NA
        }
        virginQueen <- setMisc(x = virginQueen, node = "pHomBrood", value = val)

        if (isPop(x)) {
          ret[[virgin]] <- virginQueen
        } else if (isColony(x)) {
          x <- reQueen(x = x, queen = virginQueen, simParamBee = simParamBee)
          x <- removeVirginQueens(x, simParamBee = simParamBee)
          ret <- x
       }
      }
    }
    if (isPop(x)) {
      ret <- mergePops(ret)
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    if (nCol == 0) {
      ret <- createMultiColony(simParamBee = simParamBee)
    } else {
      ret <- createMultiColony(n = nCol, simParamBee = simParamBee)
      for (colony in seq_len(nCol)) {
        if (oneColony) {
          colonyDrones <- drones
        } else if (dronePackages) {
          colonyDrones <- drones[[colony]]
        } else {
          if (crossPlan_colonyID) {
            colonyDrones <- NULL
          } else if(crossPlan_droneID) {
            colonyDrones <- drones
          }
        }
        ret[[colony]] <- cross(
          x = x[[colony]],
          drones = colonyDrones,
          crossPlan = crossPlan,
          droneColonies = droneColonies,
          nDrones = nDrones,
          spatial = spatial,
          radius = radius,
          checkCross = checkCross,
          simParamBee = simParamBee
        )
      }
    }
  }
  validObject(ret)
  return(ret)
}

#' @rdname setQueensYearOfBirth
#' @title Set the queen's year of birth
#'
#' @description Level 1 function that sets the queen's year of birth.
#'
#' @param x \code{\link{Pop-class}} (one or more than one queen),
#'   \code{\link{Colony-class}} (one colony), or
#'   \code{\link{MultiColony-class}} (more colonies)
#' @param year integer, the year of the birth of the queen
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{Pop-class}}, \code{\link{Colony-class}}, or
#'   \code{\link{MultiColony-class}} with queens having the year of birth set
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1], nInd = 1000)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson)
#'
#' # Create a Colony and a MultiColony class
#' colony <- createColony(x = basePop[2])
#' colony <- cross(x = colony, drones = droneGroups[[1]])
#' apiary <- createMultiColony(basePop[3:4], n = 2)
#' apiary <- cross(apiary, drones = droneGroups[c(2, 3)])
#'
#' # Example on Colony class
#' getQueenYearOfBirth(colony)
#' getQueenYearOfBirth(apiary)
#'
#' queen1 <- getQueen(colony)
#' queen1 <- setQueensYearOfBirth(queen1, year = 2022)
#' getQueenYearOfBirth(queen1)
#'
#' colony <- setQueensYearOfBirth(colony, year = 2022)
#' getQueenYearOfBirth(colony)
#'
#' apiary <- setQueensYearOfBirth(apiary, year = 2022)
#' getQueenYearOfBirth(apiary)
#' @export
setQueensYearOfBirth <- function(x, year, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (isPop(x)) {
    if (any(!(isVirginQueen(x, simParamBee = simParamBee) | isQueen(x, simParamBee = simParamBee)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    nInd <- nInd(x)
    x <- setMisc(x = x, node = "yearOfBirth", value = year)
  } else if (isColony(x)) {
    if (isQueenPresent(x, simParamBee = simParamBee)) {
      x@queen <- setMisc(x = x@queen, node = "yearOfBirth", value = year)
    } else {
      stop("Missing queen!")
    }
  } else if (isMultiColony(x)) {
    nCol <- nColonies(x)
    for (colony in seq_len(nCol)) {
      x[[colony]]@queen <- setMisc(
        x = x[[colony]]@queen, node = "yearOfBirth",
        value = year
      )
    }
  } else {
    stop("Argument x must be a Pop, Colony or MultiColony class object!")
  }
  return(x)
}
