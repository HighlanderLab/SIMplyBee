# ---- Level 3 MultiColony Functions ----

#' @rdname createMultiColony
#' @title Create MultiColony object
#'
#' @description Level 3 function that creates a set of colonies. Usually to
#'   start a simulation.
#'
#' @param x \code{\link{Pop-class}}, virgin queens or queens for the colonies
#'   (selected at random if there are more than \code{n} in \code{Pop}, while
#'    all are used when \code{n} is \code{NULL})
#' @param n integer, number of colonies to create (if only \code{n} is
#'   given then \code{\link{MultiColony-class}} is created with \code{n}
#'   \code{NULL}) individual colony - this is mostly useful for programming)
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @details When both \code{x} and \code{n} are \code{NULL}, then a
#'   \code{\link{MultiColony-class}} with 0 colonies is created.
#'
#' @return \code{\link{MultiColony-class}}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' basePop <- createVirginQueens(founderGenomes)
#'
#' # Create 2 empty (NULL) colonies
#' apiary <- createMultiColony(n = 2)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create 3 virgin colonies
#' apiary <- createMultiColony(x = basePop, n = 3) # specify n
#' apiary <- createMultiColony(x = basePop[1:3]) # take all provided
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#'
#' # Create mated colonies by crossing
#' apiary <- createMultiColony(x = basePop[1:2], n = 2)
#' drones <- createDrones(x = basePop[3], n = 30)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 2, nDrones = 15)
#' apiary <- cross(apiary, drones = droneGroups)
#' apiary
#' apiary[[1]]
#' apiary[[2]]
#' @export
setGeneric("createMultiColony", function(multicolony, x = NULL, n = NULL, simParamBee = NULL) {
  standardGeneric("createMultiColony")
})

#' @describeIn createMultiColony
#' @export
setMethod("createMultiColony", signature(multicolony = "MultiColony"),
function(x = NULL, n = NULL, simParamBee = NULL) {
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (is.null(x)) {
    if (is.null(n)) {
      ret <- new(Class = "MultiColony")
    } else {
      ret <- new(Class = "MultiColony", colonies = vector(mode = "list", length = n))
    }
  } else {
    if (!isPop(x)) {
      stop("Argument x must be a Pop class object!")
    }
    if (any(!(isVirginQueen(x, simParamBee = simParamBee) | isQueen(x, simParamBee = simParamBee)))) {
      stop("Individuals in x must be virgin queens or queens!")
    }
    if (is.null(n)) {
      n <- nInd(x)
    }
    if (nInd(x) < n) {
      stop("Not enough individuals in the x to create n colonies!")
    }
    ret <- new(Class = "MultiColony", colonies = vector(mode = "list", length = n))
    for (colony in seq_len(n)) {
      ret[[colony]] <- createColony(x = x[colony], simParamBee = simParamBee)
    }
  }
  validObject(ret)
  return(ret)
})
#' @rdname selectColonies
#' @title Select colonies from MultiColony object
#'
#' @description Level 3 function that selects colonies from
#'   MultiColony object based on colony ID or random selection.
#'   Whilst user can provide all three arguments ID, p and n, there is a priority
#'   list: ID takes first priority. If no ID is provided, p takes precedence over n.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#' @param ID character or numeric, ID of a colony (one or more) to be
#'   selected
#' @param n numeric, number of colonies to select
#' @param p numeric, percentage of colonies selected (takes precedence
#'   over \code{n})
#' @param by matrix, matrix of values to select by with names being
#'   colony IDs (can be obtained with \code{\link{calcColonyValue}}.
#'   If NULL, the colonies are selected at random.
#'   This parameter is used in combination
#'   with \code{n} or \code{p} to determine the number of selected colonies, and
#'   \code{selectTop} to determine whether to select the best or the worst colonies.
#' @param selectTop logical, selects highest (lowest) values if \code{TRUE} (\code{FALSE})
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{MultiColony-class}} with selected colonies
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' mean <- c(10, 10 / SP$nWorkers)
#' varA <- c(1, 1 / SP$nWorkers)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(3, 3 / SP$nWorkers)
#' varA / (varA + varE)
#' SP$addTraitADE(nQtlPerChr = 100,
#'                mean = mean,
#'                var = varA, corA = corA,
#'                meanDD = 0.1, varDD = 0.2, corD = corA,
#'                relAA = 0.1, corAA = corA)
#' SP$setVarE(varE = varE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#' apiary <- createMultiColony(basePop[2:5], n = 4)
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' apiary <- buildUp(apiary)
#' getId(apiary)
#'
#' getId(selectColonies(apiary, ID = 1))
#' getId(selectColonies(apiary, ID = c("3", "4")))
#' # ... alternative
#' getId(apiary[1])
#' getId(apiary[["4"]])
#'
#' # Select a random number of colonies
#' selectColonies(apiary, n = 3)
#' # Select a percentage of colonies
#' selectColonies(apiary, p = 0.2)
#'
#' # Since selection is random, you would get a different set of colonies with
#' # each function call
#' getId(selectColonies(apiary, p = 0.5))
#' getId(selectColonies(apiary, p = 0.5))
#'
#' # How to select colonies based on colony values?
#' # Obtain colony phenotype
#' colonyPheno <- calcColonyPheno(apiary)
#' # Select the best colony
#' selectColonies(apiary, n = 1, by = colonyPheno)
#'
#' # Select the worst 2 colonies
#' selectColonies(apiary, n = 2, by = colonyPheno, selectTop = FALSE)
#'
#' # Select best colony based on queen's genetic value for trait 1
#' queenGv <- calcColonyGv(apiary, FUN = mapCasteToColonyGv, workersTrait = NULL)
#' selectColonies(apiary, n = 1, by = queenGv)
#'
#' @export
setGeneric("selectColonies", function(multicolony, ID = NULL, n = NULL, p = NULL,
                                       by = NULL, selectTop = TRUE, simParamBee = NULL) {
  standardGeneric("selectColonies")
})

#' @describeIn selectColonies
#' @export
setMethod("selectColonies", signature(multicolony = "MultiColony"),
function(multicolony, ID = NULL, n = NULL, p = NULL,
         by = NULL, selectTop = TRUE, simParamBee = NULL) {
  if (!isMultiColony(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (!is.null(ID)) {
    if (!(is.character(ID) | is.numeric(ID))) {
      stop("ID must be character or numeric!")
    }
    trueID <- ID %in% getId(multicolony)
    if (!all(trueID)) {
      ID <- ID[trueID]
      warning("ID parameter contains come invalid IDs!")
    }
  }
  if (!is.null(n)) {
    if (n > nColonies(multicolony)) {
      stop("n must not be larger than the number of colonies in multicolony!")
    } else if (n < 0) {
      stop("n must be non-negative!")
    }
  }
  if (!is.null(p)) {
    if (1 < p) {
      stop("p must not be higher than 1!")
    } else if (p < 0) {
      stop("p must not be less than 0!")
    }
  }
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(ID)) {
    ID <- as.character(ID)
    ret <- multicolony[ID]
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(multicolony)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    if (is.null(by)) {
      lSel <- sample.int(n = nCol, size = n)
      message(paste0("Randomly selecting colonies: ", n))
    } else {
      lSel <- rownames(by)[order(by, decreasing = selectTop)[1:n]]
    }
    if (length(lSel) > 0) {
      ret <- multicolony[lSel]
    } else {
      ret <- createMultiColony(simParamBee = simParamBee)
    }
  } else {
    stop("Provide either ID, n, or p!")
  }
  validObject(ret)
  return(ret)
})

#' @rdname pullColonies
#' @title Pull out some colonies from the MultiColony object
#'
#' @description Level 3 function that pulls out some colonies
#'   from the MultiColony based on colony ID or random selection.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#' @param ID character or numeric, ID of a colony (one or more) to be pulled
#'   out
#' @param n numeric, number of colonies to select
#' @param p numeric, percentage of colonies pulled out (takes precedence
#'   over \code{n})
#' @param by matrix, matrix of values to select by with names being
#'   colony IDs (can be obtained with \code{\link{calcColonyValue}}.
#'   If NULL, the colonies are pulled at random.
#'   This parameter is used in combination
#'   with \code{n} or \code{p} to determine the number of pulled colonies, and
#'   \code{pullTop} to determine whether to pull the best or the worst colonies.
#' @param pullTop logical, pull highest (lowest) values if \code{TRUE} (\code{FALSE})
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return list with two \code{\link{MultiColony-class}}, the \code{pulled}
#'   and the \code{remnant}
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' mean <- c(10, 10 / SP$nWorkers)
#' varA <- c(1, 1 / SP$nWorkers)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(3, 3 / SP$nWorkers)
#' varA / (varA + varE)
#' SP$addTraitADE(nQtlPerChr = 100,
#'                mean = mean,
#'                var = varA, corA = corA,
#'                meanDD = 0.1, varDD = 0.2, corD = corA,
#'                relAA = 0.1, corAA = corA)
#' SP$setVarE(varE = varE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#' apiary <- createMultiColony(basePop[2:5], n = 4)
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' apiary <- buildUp(apiary)
#' getId(apiary)
#'
#' tmp <- pullColonies(apiary, ID = c(1, 2))
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' tmp <- pullColonies(apiary, ID = c("3", "4"))
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' tmp <- pullColonies(apiary, n = 2)
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' tmp <- pullColonies(apiary, p = 0.75)
#' getId(tmp$pulled)
#' getId(tmp$remnant)
#'
#' # How to pull out colonies based on colony values?
#' colonyGv <- calcColonyGv(apiary)
#' pullColonies(apiary, n = 1, by = colonyGv)
#' @export
setGeneric("pullColonies", function(multicolony, ID = NULL, n = NULL, p = NULL,
                                    by = NULL, pullTop = TRUE, simParamBee = NULL) {
  standardGeneric("pullColonies")
})

#' @describeIn pullColonies
#' @export
setMethod("pullColonies", signature(multicolony = "MultiColony"),
function(multicolony, ID = NULL, n = NULL, p = NULL,
         by = NULL, pullTop = TRUE, simParamBee = NULL) {
  if (!isMultiColony(multicolony)) {
    stop("Argument multicolony must be a MultiColony class object!")
  }
  if (is.null(simParamBee)) {
    simParamBee <- get(x = "SP", envir = .GlobalEnv)
  }
  if (!is.null(ID)) {
    trueID <- ID %in% getId(multicolony)
    if (!all(trueID)) {
      ID <- ID[trueID]
      warning("ID parameter contains come invalid IDs!")
    }
    pulled <- selectColonies(multicolony, ID,
                             simParamBee = simParamBee) # selectColonies does the checking of the IDs
    remnant <- removeColonies(multicolony, ID, simParamBee = simParamBee)
  } else if (!is.null(n) | !is.null(p)) {
    nCol <- nColonies(multicolony)
    if (!is.null(p)) {
      n <- round(nCol * p)
    }
    if (is.null(by)) {
      positions <- 1:nCol
      lPull <- sample.int(n = nCol, size = n)
      lStay <- positions[!positions %in% lPull]
      message(paste0("Randomly pulling colonies: ", n))
    } else {
      IDs <- getId(multicolony)
      lPull <- rownames(by)[order(by, decreasing = pullTop)[1:n]]
      lStay <- as.character(IDs[!IDs %in% lPull])
    }
    if (length(lPull) > 0) {
      pulled <- multicolony[lPull]
      remnant <- multicolony[lStay]
    } else {
      pulled <- createMultiColony(simParamBee = simParamBee)
      remnant <- multicolony
    }
  } else {
    stop("You must provide either ID, n, or p!")
  }
  ret <- list(pulled = pulled, remnant = remnant)
  validObject(ret$pulled)
  validObject(ret$remnant)
  return(ret)
})

#' @rdname removeColonies
#' @title Remove some colonies from the MultiColony object
#'
#' @description Level 3 function that removes some colonies
#'   from the MultiColony object based on their ID.
#'
#' @param multicolony \code{\link{MultiColony-class}}
#' @param ID character or numeric, ID of a colony (one or more) to be
#'   removed
#' @param n numeric, number of colonies to remove
#' @param p numeric, percentage of colonies removed (takes precedence
#'   over \code{n})
#' @param by matrix, matrix of values to select by with names being
#'   colony IDs (can be obtained with \code{\link{calcColonyValue}}.
#'   If NULL, the colonies are removed at random.
#'   This parameter is used in combination
#'   with \code{n} or \code{p} to determine the number of removed colonies, and
#'   \code{removeTop} to determine whether to remove the best or the worst colonies.
#' @param removeTop logical, remove highest (lowest) values if \code{TRUE} (\code{FALSE})
#' @param simParamBee \code{\link{SimParamBee}}, global simulation parameters
#'
#' @return \code{\link{MultiColony-class}} with some colonies removed
#'
#' @examples
#' founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
#' SP <- SimParamBee$new(founderGenomes)
#' \dontshow{SP$nThreads = 1L}
#' mean <- c(10, 10 / SP$nWorkers)
#' varA <- c(1, 1 / SP$nWorkers)
#' corA <- matrix(data = c(
#'   1.0, -0.5,
#'   -0.5, 1.0
#' ), nrow = 2, byrow = TRUE)
#' varE <- c(3, 3 / SP$nWorkers)
#' varA / (varA + varE)
#' SP$addTraitADE(nQtlPerChr = 100,
#'                mean = mean,
#'                var = varA, corA = corA,
#'                meanDD = 0.1, varDD = 0.2, corD = corA,
#'                relAA = 0.1, corAA = corA)
#' SP$setVarE(varE = varE)
#'
#' basePop <- createVirginQueens(founderGenomes)
#'
#' drones <- createDrones(x = basePop[1:4], nInd = 100)
#' droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10)
#' apiary <- createMultiColony(basePop[2:5], n = 4)
#' apiary <- cross(apiary, drones = droneGroups[1:4])
#' apiary <- buildUp(apiary)
#' getId(apiary)
#'
#' getId(removeColonies(apiary, ID = 1))
#' getId(removeColonies(apiary, ID = c("3", "4")))
#'
#' nColonies(apiary)
#' apiary <- removeColonies(apiary, ID = "2")
#' nColonies(apiary)
#'
#' # How to remove colonies based on colony values?
#' # Obtain colony phenotype
#' colonyPheno <- calcColonyPheno(apiary)
#' # Remove the worst colony
#' removeColonies(apiary, n = 1, by = colonyPheno)
#'
#' @export
setGeneric("removeColonies", function(multicolony,  ID = NULL, n = NULL, p = NULL,
                                      by = NULL, removeTop = FALSE, simParamBee = NULL) {
  standardGeneric("removeColonies")
})

#' @describeIn removeColonies
#' @export
setMethod("removeColonies", signature(multicolony = "MultiColony"),
          function(multicolony,  ID = NULL, n = NULL, p = NULL,
                   by = NULL, removeTop = FALSE, simParamBee = NULL) {
            if (!isMultiColony(multicolony)) {
              stop("Argument multicolony must be a MultiColony class object!")
            }
            if (is.null(simParamBee)) {
              simParamBee <- get(x = "SP", envir = .GlobalEnv)
            }
            if (!is.null(ID)) {
              trueID <- ID %in% getId(multicolony)
              if (!all(trueID)) {
                ID <- ID[trueID]
                warning("ID parameter contains come invalid IDs!")
              }
              ret <- selectColonies(multicolony,
                                    ID = getId(multicolony)[!getId(multicolony) %in% ID],
                                    simParamBee = simParamBee)
            } else if (!is.null(n) | !is.null(p)) {
              nCol <- nColonies(multicolony)
              if (!is.null(p)) {
                n <- round(nCol * p)
              }
              if (is.null(by)) {
                lSel <- sample.int(n = nCol, size = (nCol - n))
                message(paste0("Randomly removing colonies: ", n))
              } else {
                lSel <- rownames(by)[order(by, decreasing = !removeTop)[1:(nCol - n)]]
              }
              if (length(lSel) > 0) {
                ret <- multicolony[lSel]
              } else {
                ret <- multicolony
              }
            } else {
              stop("You must provide either ID, n, or p!")
            }
            validObject(ret)
            return(ret)
          })
