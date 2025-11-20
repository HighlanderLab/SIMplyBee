# ---- nColonies ----
test_that("nColonies", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  expect_equal(nColonies(createMultiColony(n = 2, simParamBee = SP)), 2)
  expect_equal(nColonies(createMultiColony(simParamBee = SP)), 0)
  expect_equal(nColonies(createMultiColony(n = 10, simParamBee = SP)), 10)
})


# ---- nCaste ----
test_that("nCaste", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)

  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 45, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 3, nDrones = 10, simParamBee = SP)
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nDrones = 10, nWorkers = 20, simParamBee = SP)
  expect_equal(nCaste(colony, caste = "queen", simParamBee = SP), 1)
  expect_equal(nCaste(colony, caste = "drones", simParamBee = SP), 10)
  expect_equal(nCaste(colony, caste = "virginQueens", simParamBee = SP), 0)
  expect_equal(nCaste(colony, caste = "fathers", simParamBee = SP), 10)

  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
  apiary <- buildUp(apiary, nWorkers = 20, nDrones = 10, simParamBee = SP)
  expect_equal(sum(nCaste(apiary, caste = "queen", simParamBee = SP)), 2)
  expect_equal(sum(nCaste(apiary, caste = "virginQueens", simParamBee = SP)), 0)
  expect_equal(sum(nCaste(apiary, caste = "fathers", simParamBee = SP)), 20)
})

# ---- nQueens ----

test_that("nQueens", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  colony1 <- createColony(x = basePop[1], simParamBee = SP)
  colony1 <- cross(colony1, drones = fatherGroups[[1]], simParamBee = SP)
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = fatherGroups[c(2, 3)], simParamBee = SP)

  expect_equal(nQueens(colony1, simParamBee = SP), 1)
  colony1 <- removeQueen(colony1, simParamBee = SP)
  expect_equal(nQueens(colony1, simParamBee = SP), 0)
  expect_equal(sum(nQueens(apiary, simParamBee = SP)), 2)
})

# ---- nDrones ----
test_that("nDrones", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  colony1 <- createColony(x = basePop[1], simParamBee = SP)
  colony1 <- cross(colony1, drones = fatherGroups[[1]], simParamBee = SP)
  colony2 <- createColony(x = basePop[2], simParamBee = SP)
  colony2 <- cross(colony2, drones = fatherGroups[[2]], simParamBee = SP)


  expect_equal(nDrones(colony1, simParamBee = SP), 0)
  colony1 <- addDrones(colony1, nInd = 5, simParamBee = SP)
  colony2 <- addDrones(colony2, nInd = 10, simParamBee = SP)
  expect_equal(nDrones(colony1, simParamBee = SP), 5)
  expect_equal(nDrones(colony2, simParamBee = SP), 10)
  expect_equal(sum(nDrones(c(colony1, colony2), simParamBee = SP)), 15)
  expect_error(nDrones(basePop, simParamBee = SP))
})

# ---- isGenoHeterozygous ----

####----- isGenoHeterozygous ---- ####
test_that("isGenoHeterozygous", {
  geno <- matrix(
    data = c(
      0, 1, 2,
      0, 0, 0,
      2, 2, 2
    ),
    nrow = 3, byrow = TRUE
  )
  expect_true(SIMplyBee:::isGenoHeterozygous(geno[1, , drop = FALSE]))
  expect_false(SIMplyBee:::isGenoHeterozygous(geno[2, , drop = FALSE]))
  expect_false(SIMplyBee:::isGenoHeterozygous(geno[3, , drop = FALSE]))
  expect_equal(SIMplyBee:::isGenoHeterozygous(geno), c(TRUE, FALSE, FALSE))
})

# ---- isCaste ----

test_that("isCaste", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)

  # get the queen that is a caste "queen" and ask if it is a caste "queen"
  # and drones and drones
  expect_true(isCaste(getQueen(colony, simParamBee = SP), caste = "queen", simParamBee = SP))
  expect_true(all(isCaste(getDrones(colony, simParamBee = SP), caste = "drones", simParamBee = SP)))
  expect_true(all(isCaste(getDrones(colony, simParamBee = SP), caste = "drones", simParamBee = SP)))
  # get the queen that is a caste "queen" and test if it is a caste "workers",
  #test on virgin queen that is not present in a colony
  expect_false(isCaste(getQueen(colony, simParamBee = SP), caste = "workers", simParamBee = SP))
  #test on virgin queen that is not present in a colony
  expect_null(isCaste(getVirginQueens(colony, simParamBee = SP), caste = "virginQueens", simParamBee = SP))
  malePop <- c(getDrones(colony, simParamBee = SP), getFathers(colony, simParamBee = SP))
  expect_true(any(isCaste(malePop, caste = "drones", simParamBee = SP)))
  expect_true(any(isCaste(malePop, caste = "fathers", simParamBee = SP)))
  expect_false(all(isCaste(malePop, caste = "drones", simParamBee = SP)))
  expect_false(all(isCaste(malePop, caste = "fathers", simParamBee = SP)))
})

# ---- calcQueensPHomBrood ----

test_that("calcQueensPHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)

  expect_error(calcQueensPHomBrood(colony@drones, simParamBee = SP))
  expect_error(calcQueensPHomBrood(colony@workers, simParamBee = SP))
  expect_true(is.numeric(calcQueensPHomBrood(colony@queen, simParamBee = SP)))

  colony@queen <- NULL
  expect_error(calcQueensPHomBrood(colony@queen, simParamBee = SP))
  apiary <- createMultiColony(simParamBee = SP)
  colony@workers <- NULL
  colony@drones <- NULL
  colony@virginQueens <- NULL
  expect_error(calcQueensPHomBrood(colony, simParamBee = SP))
  expect_equal((length(calcQueensPHomBrood(apiary, simParamBee = SP))), 0)
})

# ---- pHomBrood ----

test_that("pHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)

  expect_error(pHomBrood(colony@workers, simParamBee = SP))
  expect_error(pHomBrood(colony@virginQueens, simParamBee = SP))
  expect_error(pHomBrood(colony@drones, simParamBee = SP))
  #expect_true(is.numeric(pHomBrood(colony@queen, simParamBee = SP)))

  colony@queen <- NULL
  expect_error(pHomBrood(colony@queen, simParamBee = SP))
  apiary <- createMultiColony(simParamBee = SP)
  colony@workers <- NULL
  colony@drones <- NULL
  colony@virginQueens <- NULL
  expect_error(pHomBrood(colony, simParamBee = SP))
  expect_equal(length(pHomBrood(apiary, simParamBee = SP)), 0)
})

#---- nHomBrood -----

test_that("nHomBrood", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)

  expect_error(nHomBrood(colony@workers, simParamBee = SP))
  expect_error(nHomBrood(colony@virginQueens, simParamBee = SP))
  expect_error(nHomBrood(colony@drones, simParamBee = SP))
  expect_true(is.numeric(nHomBrood(colony@queen, simParamBee = SP)))

  colony@queen <- NULL
  expect_error(nHomBrood(colony@queen, simParamBee = SP))
  apiary <- createMultiColony(simParamBee = SP)
  colony@workers <- NULL
  colony@drones <- NULL
  colony@virginQueens <- NULL
  expect_error(nHomBrood(colony, simParamBee = SP))
  expect_equal(length(nHomBrood(apiary, simParamBee = SP)), 0)
})

# ---- isQueenPresent ----

test_that("isQueenPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1, simParamBee = SP)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony(simParamBee = SP)
  colony2 <- createColony(simParamBee = SP)

  expect_true(isQueenPresent(colony, simParamBee = SP))
  expect_false(isQueenPresent(apiary2, simParamBee = SP))
  expect_error(isQueenPresent(vec, simParamBee = SP))
  expect_true(is.vector(isQueenPresent(apiary2, simParamBee = SP)))
  expect_false(isQueenPresent(apiary2, simParamBee = SP))
  expect_false(isQueenPresent(colony2, simParamBee = SP))
})

# ---- isVirginQueensPresent ----

test_that("isVirginQueensPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1, simParamBee = SP)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony(simParamBee = SP)
  colony2 <- createColony(simParamBee = SP)


  expect_true(isVirginQueensPresent(colony, simParamBee = SP))
  expect_false(isVirginQueensPresent(apiary2, simParamBee = SP))
  expect_error(isVirginQueenPresent(vec, simParamBee = SP))
  expect_true(is.vector(isVirginQueensPresent(apiary2, simParamBee = SP)))
  expect_false(isVirginQueensPresent(apiary2, simParamBee = SP))
  expect_false(isVirginQueensPresent(colony2, simParamBee = SP))
})

# ---- isProductive ----

test_that("isProductive", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)

  expect_false(isProductive(colony))
  colony <- buildUp(x = colony, simParamBee = SP)
  expect_true(isProductive(colony))

  apiary <- createMultiColony(basePop[3:4], simParamBee = SP)
  apiary <- cross(apiary, drones = fatherGroups[c(2, 3)], simParamBee = SP)

  expect_false(all(isProductive(apiary)))
  apiary <- buildUp(x = apiary, simParamBee = SP)
  expect_true(all(isProductive(apiary)))

  colony <- createColony(simParamBee = SP)
  expect_false(isProductive(colony))
  colony <- NULL
  expect_error(isProductive(colony))

  apiary <- createMultiColony(simParamBee = SP)
  expect_true(is.list(isProductive(apiary)))
})
# ---- reduceDroneHaplo ----

test_that("reduceDroneHaplo", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 2, simParamBee = SP)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteHaplo(virginQueens, simParamBee = SP)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteHaplo(drones, simParamBee = SP)
  mix <- rbind(tmp, tmpD)

  expect_equal(nrow(reduceDroneHaplo(haplo = mix, pop = c(drones, virginQueens))), 6)
  expect_equal(nrow(reduceDroneHaplo(haplo = tmpD, pop = drones)), 2)
  expect_error(reduceDroneHaplo(haplo = vec, pop = drones))
  expect_error(reduceDroneHaplo(haplo = df, pop = drones))
  expect_true(is.matrix(reduceDroneHaplo(haplo = tmpD, pop = drones)))
  tmp <- reduceDroneHaplo(haplo = tmpD, pop = drones)
  expect_true(all(rle(as.vector(tmp))$values %in% 0:1))
})

# ---- reduceDroneGeno ----

test_that("reduceDroneGeno", {
  founderGenomes <- quickHaplo(nInd = 10, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 2, simParamBee = SP)
  virginQueens <- c(basePop[2:3])
  vec <- c(1,2,"a")
  tmp <- getSegSiteGeno(virginQueens, simParamBee = SP)
  df <- as.data.frame(tmp)
  tmpD <- getSegSiteGeno(drones, simParamBee = SP)


  expect_equal(nrow(reduceDroneGeno(geno = tmpD, pop = drones)), 2)
  expect_error(reduceDroneGeno(geno = vec, pop = drones))
  expect_error(reduceDroneGeno(geno =  df, pop = drones))
  expect_true(is.matrix(reduceDroneGeno(geno = tmpD, pop = drones)))
  tmp <- reduceDroneGeno(geno = tmpD, pop = drones)
  expect_true(all(rle(as.vector(tmp))$values %in% 0:1))
})

# ---- getCsdAlleles ----

test_that("getCsdAlleles", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2],simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_true(is.list(getCsdAlleles(colony, simParamBee = SP)))
  expect_true(is.matrix(getCsdAlleles(getQueen(colony, simParamBee = SP), simParamBee = SP)))

  # set CSD to NULL
  rm(SP)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(getCsdAlleles(colony, simParamBee = SP))

  # test unique and colapse
  rm(SP)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)
  expect_true(is.matrix(getCsdAlleles(colony, collapse = TRUE, simParamBee = SP)))
  expect_equal(nrow(getCsdAlleles(colony, collapse = TRUE, simParamBee = SP)),
               sum(nQueens(colony, simParamBee = SP)*2, nDrones(colony, simParamBee = SP),
                   nWorkers(colony, simParamBee = SP)*2, nFathers(colony, simParamBee = SP),
                   nVirginQueens(colony, simParamBee = SP)*2))
  expect_equal(nrow(getCsdAlleles(colony, collapse = TRUE, unique = TRUE, simParamBee = SP)),
               nrow(unique(getCsdAlleles(colony, collapse = TRUE, simParamBee = SP))))
  expect_true(nrow(getCsdAlleles(colony, collapse = TRUE, unique = TRUE, simParamBee = SP)) <= SP$nCsdAlleles)

})

# ---- getCsdGeno ----

test_that("getCsdGeno", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_true(is.list(getCsdGeno(colony, simParamBee = SP)))
  expect_true(is.matrix(getCsdGeno(getQueen(colony, simParamBee = SP), simParamBee = SP)))
  expect_true(nCsdAlleles(colony, collapse = TRUE, simParamBee = SP) <= SP$nCsdAlleles)

  geno <- getCsdGeno(colony, simParamBee = SP)
  expect_equal(nrow(geno$fathers), 10)

  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(getCsdGeno(colony, simParamBee = SP))

})

# ---- isCsdHeterozygous ----

test_that("isCsdHeterozygous", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = 5)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)
  colony@virginQueens <- createVirginQueens(colony, nInd = 1, simParamBee = SP)

  expect_true(isCsdHeterozygous(colony@queen, simParamBee = SP))
  expect_true(is.vector(isCsdHeterozygous(colony@workers, simParamBee = SP)))
  expect_true(all(isCsdHeterozygous(colony@drones, simParamBee = SP)))
  expect_true(isCsdHeterozygous(colony@virginQueens, simParamBee = SP))

  # set CSD to NULL
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(isCsdHeterozygous(colony@queen, simParamBee = SP))
})

# ---- nCsdAlleles ----

test_that("nCsdAlleles", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_equal(nCsdAlleles(colony@queen, simParamBee = SP), 2)
  expect_equal(nCsdAlleles(colony@workers[10], simParamBee = SP), 2)
  expect_equal(nCsdAlleles(colony@drones[9], simParamBee = SP), 1)
  expect_true(is.integer(nCsdAlleles(colony@queen, simParamBee = SP)))

  # set CSD to NULL
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)

  expect_error(nCsdAlleles(colony@queen, simParamBee = SP))

  #collapse argument
  nCsdAlleles <- 5
  SP <- SimParamBee$new(founderGenomes, nCsdAlleles = nCsdAlleles)
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, simParamBee = SP)
  expect_true(is.numeric(nCsdAlleles(colony, collapse = TRUE, simParamBee = SP)))
  expect_true(nCsdAlleles(colony, collapse = TRUE, simParamBee = SP) <= nCsdAlleles)
})

# ---- calcBeeGRMIbs ----

test_that("calcBeeGRMIbs", {
  founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  SP$setTrackRec(TRUE)
  SP$setTrackPed(isTrackPed = TRUE)
  SP$addTraitA(10)
  SP$addSnpChip(5)

  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)
  apiary <- createMultiColony(basePop[2:3], n = 2, simParamBee = SP)
  apiary <- cross(x = apiary, drones = fatherGroups[c(2, 3)], simParamBee = SP)
  apiary <- buildUp(x = apiary, simParamBee = SP)
  apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

  genoQ <- getQueenSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoF <- getFathersSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoW <- getWorkersSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoD <- getDronesSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoV <- getVirginQueensSegSiteGeno(apiary[[1]], simParamBee = SP)
  genoMeanW <- apply(X = genoW, MARGIN = 2, FUN = mean)
  genoMeanD <- apply(X = genoD, MARGIN = 2, FUN = mean)

  geno <- rbind(genoQ, genoF, genoW, genoD, genoV, genoMeanW, genoMeanD)
  n <- length(rownames(geno))
  rownames(geno)[c(n - 1, n)] <- c("mw", "md")

  sex <- getCasteSex(x = apiary[[1]], simParamBee = SP)
  sex <- c(
    sex$queen, sex$fathers, sex$workers, sex$drones, sex$virginQueens,
    "F", "M"
  )
  GRM <- calcBeeGRMIbs(x = geno, sex = sex)

  expect_true(is.matrix(GRM))

  # added a vector since x must be a matrix

  vec <- c(1,2,"a")

  expect_error(calcBeeGRMIbs(x = vec, sex = sex))

  # added A and B into the sex since it can contain only M and F

  sex <- getCasteSex(x = apiary[[1]], simParamBee = SP)
  sex <- c(
    sex$queen, sex$drones, sex$workers, sex$drones, sex$virginQueens,
    "A", "B"
  )

  expect_error(calcBeeGRMIbs(x = GRM, sex = sex))
})

# ---- editCsdLocus ----


test_that("editCsdLocus", {
  founderGenomes <- quickHaplo(nInd = 100, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, editCsd = FALSE, simParamBee = SP)
  nrow(getCsdAlleles(basePop, unique = TRUE, simParamBee = SP))
  expect_false(all(isCsdHeterozygous(basePop, simParamBee = SP)))

  basePopEdited <- SIMplyBee:::editCsdLocus(basePop, simParamBee = SP)

  expect_true(isPop(basePopEdited))
  expect_true(all(isCsdHeterozygous(basePopEdited, simParamBee = SP)))
})

# ---- emptyNULL ----
test_that("emptyNULL", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
  basePop <- createVirginQueens(founderGenomes, editCsd = FALSE, simParamBee = SP)

  expect_true(isEmpty(new(Class = "Pop")))
  expect_true(isEmpty(basePop[0]))
  expect_false(isEmpty(basePop))

  emptyColony <- createColony(simParamBee = SP)
  nonEmptyColony <- createColony(basePop[1], simParamBee = SP)
  expect_true(isEmpty(emptyColony))
  expect_false(isEmpty(nonEmptyColony))

  emptyApiary <- createMultiColony(n = 3, simParamBee = SP)
  emptyApiary1 <- c(createColony(simParamBee = SP), createColony(simParamBee = SP))
  nonEmptyApiary <- createMultiColony(basePop[2:5], n = 4, simParamBee = SP)

  expect_true(all(isEmpty(emptyApiary)))
  expect_true(all(isEmpty(emptyApiary1)))
  expect_true(!all(isEmpty(nonEmptyApiary)))
  expect_true(all(isNULLColonies(emptyApiary)))
  expect_true(!all(isNULLColonies(emptyApiary1)))
  expect_true(!all(isNULLColonies(nonEmptyApiary)))

  expect_equal(nEmptyColonies(emptyApiary), 3)
  expect_equal(nEmptyColonies(emptyApiary1), 2)
  expect_equal(nEmptyColonies(nonEmptyApiary), 0)
  expect_equal(nNULLColonies(emptyApiary), 3)
  expect_equal(nNULLColonies(emptyApiary1), 0)
  expect_equal(nNULLColonies(nonEmptyApiary), 0)
})

# ---- isDronesPresent ----

test_that("isDronesPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1, simParamBee = SP)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony(simParamBee = SP)

  expect_true(isDronesPresent(colony, simParamBee = SP))
  expect_error(isDronesPresent(vec, simParamBee = SP))
  expect_true(is.vector(isDronesPresent(apiary2, simParamBee = SP)))
})

# ---- isFathersPresent ----

test_that("isFathersPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1, simParamBee = SP)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony(simParamBee = SP)

  expect_true(isFathersPresent(colony, simParamBee = SP))
  expect_false(isFathersPresent(apiary, simParamBee = SP))
  expect_error(isFathersPresent(vec, simParamBee = SP))
  expect_true(is.vector(isFathersPresent(apiary2, simParamBee = SP)))
  queen <- colony@queen
  expect_error(isFathersPresent(queen))
})

# ---- isWorkersPresent ----

test_that("isWorkersPresent", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  fatherGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = 10, simParamBee = SP)

  # Create a Colony class object
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = fatherGroups[[1]], simParamBee = SP)
  colony <- buildUp(x = colony, nWorkers = 120, nDrones = 20, simParamBee = SP)
  colony <- addVirginQueens(x = colony, nInd = 1, simParamBee = SP)
  apiary <- createMultiColony(n = 1, simParamBee = SP)
  vec <- c(1,2,3,4)
  apiary2 <- createMultiColony(simParamBee = SP)

  expect_true(isWorkersPresent(colony, simParamBee = SP))
  expect_false(isWorkersPresent(apiary, simParamBee = SP))
  expect_error(isWorkersPresent(vec, simParamBee = SP))
  expect_true(is.vector(isWorkersPresent(apiary2, simParamBee = SP)))
})

# ---- isGenoHeterozygous ----
test_that("isGenoHeterozygous", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)
   colony <- addVirginQueens(x = colony, nInd = 4, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)
   apiary <- addVirginQueens(x = apiary, nInd = 5, simParamBee = SP)

   # Caste members taken from Colony class
   (tmp <- getCsdGeno(getQueen(colony, simParamBee = SP), simParamBee = SP))
   expect_true(SIMplyBee:::isGenoHeterozygous(tmp))

   # Caste members taken from MultiColony class
   (tmp <- getCsdGeno(getQueen(apiary[[1]], simParamBee = SP), simParamBee = SP))
   expect_true(SIMplyBee:::isGenoHeterozygous(tmp))

})

# ---- getBV ----
test_that("getBV", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
   SP$nThreads = 1L
   SP$addTraitA(nQtlPerChr = 10, var = 1)
   SP$addSnpChip(5)
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)

   expect_equal(nrow(SIMplyBee:::getBv(x = getQueen(colony, simParamBee = SP), simParamBee = SP)), 1)
   expect_equal(nrow(SIMplyBee:::getQueenBv(x = colony, simParamBee = SP)), 1)
   expect_equal(nrow(SIMplyBee:::getBv(x = getWorkers(colony, simParamBee = SP), simParamBee = SP)), 6)
   expect_equal(nrow(SIMplyBee:::getWorkersBv(x = colony, simParamBee = SP)), 6)

   expect_length(SIMplyBee:::getBv(apiary, caste = "workers", simParamBee = SP), 2)
   expect_equal(nrow(SIMplyBee:::getBv(apiary, caste = "workers", simParamBee = SP)[[1]]), 6)
})

# ---- getDd ----
test_that("getDd", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
   SP$nThreads = 1L
   SP$addTraitAD(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1)
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)

   expect_equal(nrow(SIMplyBee:::getDd(x = getQueen(colony, simParamBee = SP), simParamBee = SP)), 1)
   expect_equal(nrow(SIMplyBee:::getQueenDd(x = colony, simParamBee = SP)), 1)
   expect_equal(nrow(SIMplyBee:::getDd(x = getWorkers(colony, simParamBee = SP), simParamBee = SP)), 6)
   expect_equal(nrow(SIMplyBee:::getWorkersDd(x = colony, simParamBee = SP)), 6)

   expect_length(SIMplyBee:::getDd(apiary, caste = "workers", simParamBee = SP), 2)
   expect_equal(nrow(SIMplyBee:::getDd(apiary, caste = "workers", simParamBee = SP)[[1]]), 6)
})

# ---- getAa ----
test_that("getAa", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
   SP$nThreads = 1L
   SP$addTraitADE(nQtlPerChr = 10, meanDD = 0.2, varDD = 0.1, relAA = 0.5)
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

   # Create a Colony and a MultiColony class
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   colony <- buildUp(x = colony, nWorkers = 6, nDrones = 3, simParamBee = SP)

   apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
   apiary <- buildUp(x = apiary, nWorkers = 6, nDrones = 3, simParamBee = SP)

   expect_equal(nrow(SIMplyBee:::getAa(x = getQueen(colony, simParamBee = SP), simParamBee = SP)), 1)
   expect_equal(nrow(SIMplyBee:::getQueenAa(x = colony, simParamBee = SP)), 1)
   expect_equal(nrow(SIMplyBee:::getAa(x = getWorkers(colony, simParamBee = SP), simParamBee = SP)), 6)
   expect_equal(nrow(SIMplyBee:::getWorkersAa(x = colony, simParamBee = SP)), 6)

   expect_length(SIMplyBee:::getAa(apiary, caste = "workers", simParamBee = SP), 2)
   expect_equal(nrow(SIMplyBee:::getAa(apiary, caste = "workers", simParamBee = SP)[[1]]), 6)
})

# ---- editCsdLocus ----
test_that("editCsdLocus", {
   founderGenomes <- quickHaplo(nInd = 100, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = 1, nCsdAlleles = 8)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, editCsd = FALSE, simParamBee = SP)
   nrow(getCsdAlleles(basePop, unique = TRUE, simParamBee = SP))
   all(isCsdHeterozygous(basePop, simParamBee = SP))

   basePopEdited <- SIMplyBee:::editCsdLocus(basePop, simParamBee = SP)
   nrow(getCsdAlleles(basePopEdited, unique = TRUE, simParamBee = SP))
   expect_true(all(isCsdHeterozygous(basePopEdited, simParamBee = SP)))
})

# ---- getLocation ----
test_that("getLocation", {
   founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
   drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
   droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)
   colony <- createColony(x = basePop[2], simParamBee = SP)
   colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)
   apiary <- createMultiColony(basePop[3:4], simParamBee = SP)
   apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)

   expect_equal(getLocation(colony), c(0, 0))
   expect_equal(getLocation(apiary[[1]]), c(0, 0))
   expect_equal(getLocation(apiary), list("2" = c(0, 0), "3" = c(0, 0)))
   tmp <- matrix(data = 0, nrow = 2, ncol = 2, dimnames = list(c("2", "3"), NULL))
   expect_equal(getLocation(apiary, collapse = TRUE), tmp)

   loc <- c(123, 456)
   expect_equal(getLocation(setLocation(colony, location = loc)), loc)

   expect_equal(getLocation(setLocation(apiary, location = loc)),
                list("2" = loc, "3" = loc))
})

# ---- createCrossPlan ----
test_that("createCrossPlan", {
  founderGenomes <- quickHaplo(nInd = 1000, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  # Create three virgin MultiColony objects with locations
  virginColonies1 <- createMultiColony(basePop[1:2], simParamBee = SP)
  virginColonies1 <- setLocation(virginColonies1,
                                 location = Map(c, runif(2, 0, 2*pi),
                                                runif(2, 0, 2*pi)))
  virginColonies2 <- createMultiColony(basePop[3:4], simParamBee = SP)
  virginColonies2 <- setLocation(virginColonies2,
                                 location = Map(c, runif(2, 0, 2*pi),
                                                runif(2, 0, 2*pi)))
  virginColonies3 <- createMultiColony(basePop[5:6], simParamBee = SP)
  virginColonies3 <- setLocation(virginColonies3,
                                 location = Map(c, runif(2, 0, 2*pi),
                                                runif(2, 0, 2*pi)))

  # Create drone colonies
  droneColonies <- createMultiColony(basePop[7:9], simParamBee = SP)
  droneColonies <- setLocation(droneColonies,
                               location = Map(c, runif(3, 0, 2*pi),
                                              runif(3, 0, 2*pi)))

  # Create some drones to mate initial drone colonies with
  DCA <- createDrones(basePop[10:12], nInd = 20, simParamBee = SP)
  # Cross initial virgin drone colonies to the DCA with a random cross plan
  randomCrossPlan <- createCrossPlan(x = droneColonies,
                                     drones = DCA,
                                     nDrones = 15,
                                     spatial = FALSE,
                                     simParamBee = SP)
  expect_length(randomCrossPlan, 3)
  droneColonies <- cross(droneColonies,
                         drones = DCA,
                         nDrones = nFathersPoisson,
                         crossPlan = randomCrossPlan,
                         simParamBee = SP)

  expect_equal(as.vector(nFathers(droneColonies, simParamBee = SP)), c(15, 15, 15))

  # Cross according to a spatial cross plan according to the colonies' locations
  crossPlanSpatial <- createCrossPlan(x = virginColonies1,
                                      droneColonies = droneColonies,
                                      nDrones = nFathersPoisson,
                                      spatial = TRUE,
                                      radius = 1.5,
                                      simParamBee = SP)

  expect_length(crossPlanSpatial, 2)
  expect_error(createCrossPlan(x = droneColonies, droneColonies = virginColonies1, simParamBee = SP))
})


# ---- Get Caste ----
test_that("getCaste", {
  founderGenomes <- quickHaplo(nInd = 1000, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  expect_vector(getCaste(basePop, simParamBee = SP), "virginQueens")

  #Create drones
  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  droneGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)
  expect_vector(getCaste(drones, simParamBee = SP), "drones")

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = droneGroups[[1]], simParamBee = SP)

  # test that all drones in colony are now fathers. Colony should only contain queen and fathers
  expect_vector(getCaste(getQueen(colony, simParamBee = SP), simParamBee = SP), "queen")
  expect_vector(getCaste(getFathers(colony, simParamBee = SP), simParamBee = SP), "fathers")
  expect_error(getCaste(getWorkers(colony, simParamBee = SP), simParamBee = SP))
  expect_error(getCaste(getDrones(colony, simParamBee = SP), simParamBee = SP))
  expect_error(getCaste(getVirginQueens(colony, simParamBee = SP), simParamBee = SP))

  # all caste members are now present
  colony <- buildUp(x = colony, nWorkers = 20, nDrones = 5, simParamBee = SP)
  colony <- addVirginQueens(colony, nInd = 5, simParamBee = SP)
  expect_vector(getCaste(getQueen(colony, simParamBee = SP), simParamBee = SP), "queen")
  expect_vector(getCaste(getFathers(colony, simParamBee = SP), simParamBee = SP), "fathers")
  expect_vector(getCaste(getWorkers(colony, simParamBee = SP), simParamBee = SP), "workers")
  expect_vector(getCaste(getDrones(colony, simParamBee = SP), simParamBee = SP), "drones")
  expect_vector(getCaste(getVirginQueens(colony, simParamBee = SP), simParamBee = SP), "virginQueen")

  # Check colony collapses
  expect_length(getCaste(colony, collapse = FALSE, simParamBee = SP), 5)
  expect_vector(getCaste(colony, collapse = TRUE, simParamBee = SP), c("queen", "fathers", "workers", "drones", "virginQueens"))

  # Create virgin apiary containing 2 colonies
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  expect_length(getCaste(apiary, simParamBee = SP), 2)
  expect_vector(getCaste(getVirginQueens(apiary[[1]], simParamBee = SP), simParamBee = SP), "virginQueen")

  # Mate apiaries, now only contains queens and fathers
  apiary <- cross(apiary, drones = droneGroups[c(2, 3)], simParamBee = SP)
  expect_vector(getCaste(getQueen(apiary[[1]], simParamBee = SP), simParamBee = SP), "queen")
  expect_vector(getCaste(getFathers(apiary[[1]], simParamBee = SP), simParamBee = SP), "fathers")

  # all caste members are now present in apiary
  apiary <- buildUp(x = apiary, nWorkers = 10, nDrones = 2, simParamBee = SP)
  apiary <- addVirginQueens(apiary, nInd = 4, simParamBee = SP)
  expect_vector(getCaste(getWorkers(apiary[[1]], simParamBee = SP), simParamBee = SP), "workers")
  expect_vector(getCaste(getDrones(apiary[[1]], simParamBee = SP), simParamBee = SP), "drones")
  expect_vector(getCaste(getVirginQueens(apiary[[1]], simParamBee = SP), simParamBee = SP), "virginQueen")


  bees <- c(
    getQueen(colony, simParamBee = SP),
    getFathers(colony, nInd = 2, simParamBee = SP),
    getWorkers(colony, nInd = 2, simParamBee = SP),
    getDrones(colony, nInd = 2, simParamBee = SP),
    getVirginQueens(colony, nInd = 2, simParamBee = SP)
  )
  expect_vector(getCaste(bees, simParamBee = SP), c("queen", "fathers", "workers", "drones", "virginQueens"))


})





