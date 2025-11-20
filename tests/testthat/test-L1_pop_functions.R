# ---- getCastePop ----

test_that("getCastePop", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 15, simParamBee = SP)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = drones, simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 200, nDrones = 100, simParamBee = SP)
  # Colony without workers
  colony1 <- removeWorkers(colony, simParamBee = SP)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony, simParamBee = SP), n = 2, nDrones = 15, simParamBee = SP)
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups, simParamBee = SP)
  apiary <- addWorkers(apiary, nInd = 100, simParamBee = SP)

  expect_warning(getCastePop(colony, caste = "drones", nInd = 150, simParamBee = SP))
  suppressWarnings(expect_s4_class(getCastePop(colony, caste = "drones", simParamBee = SP), "Pop"))
  expect_s4_class(getCastePop(colony, caste = "workers", simParamBee = SP), "Pop")
  expect_s4_class(getCastePop(colony, caste = "queen", simParamBee = SP), "Pop")
  expect_null(getCastePop(colony, caste = "virginQueens", simParamBee = SP))
  suppressWarnings(expect_length(getCastePop(colony, simParamBee = SP), 5))
  suppressWarnings(expect_length(getCastePop(colony1, simParamBee = SP, ), 5))
  expect_type(getCastePop(apiary, simParamBee = SP), "list")
  expect_length(getCastePop(apiary, simParamBee = SP), 2)
  # Test whether you pull out more individuals that available
  expect_equal(getCastePop(colony, caste = "workers", nInd = 10, simParamBee = SP)@nInd, 10)
  expect_equal(getCastePop(colony, caste = "workers", nInd = 100, simParamBee = SP)@nInd, 100)
})

# ---- createVirginQueens ----

test_that("createVirginQueens", {
  founderGenomes <- quickHaplo(nInd = 5, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  #check that output is virginqueens ?
  expect_true(all(isVirginQueen(createVirginQueens(founderGenomes, simParamBee = SP), simParamBee = SP)))

  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  expect_true(all(isVirginQueen(basePop, simParamBee = SP)))

  drones <- createDrones(basePop[1], n = 15, simParamBee = SP)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = drones, simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100, simParamBee = SP)

  colony1 <- createColony(basePop[3], simParamBee = SP)
  # If no queen in colony - queen is missing
  expect_error(createVirginQueens(x= colony1, nInd = 5, simParamBee = SP))

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony, simParamBee = SP), n = 2, nDrones = 15, simParamBee = SP)
  apiary <- createMultiColony(basePop[4:5], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups, simParamBee = SP)
  # Input = multicolony       Output = named list (by colony ID) of pop class
  expect_type(createVirginQueens(x= apiary, nInd = 7, simParamBee = SP), "list")
  expect_length(createVirginQueens(x= apiary, simParamBee = SP), 2)

  # Error when testing on empty MultiColony
  apiary1 <- createMultiColony(n = 2, simParamBee = SP)
  expect_error(createVirginQueens(apiary1, nInd = 5, simParamBee = SP))

  #check that output is virginqueens ?
  expect_false(all(isVirginQueen(drones, simParamBee = SP)))
  expect_false(all(isVirginQueen(basePop, simParamBee = SP)))
  expect_error(createVirginQueens(basePop, simParamBee = SP))

  # Input = pop/colony        Output = pop class
  expect_s4_class(createVirginQueens(x= colony, nInd = 5, simParamBee = SP), "Pop")
  suppressWarnings(expect_error(createVirginQueens(x= colony, nInd = 0, simParamBee = SP)))
  expect_equal(createVirginQueens(x= colony, nInd = 5, simParamBee = SP)@nInd, 5)

  # look at csd activity if editCsd = TRUE/FALSE #TODO
  # csdAlleles = NULL or list
  # if NULL = sample hetero csd genotype for each vq from all csd alleles
  # if not NULL = nInd provided with each node holding matrix/df
  # Potential error if provided incorrectly?
})

# ---- createDrones ----

test_that("createDrones", {
  founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  # Error: x can't be a MapPop
  expect_error(createDrones(founderGenomes, simParamBee = SP))

  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 15, simParamBee = SP)
  drones2 <- createDrones(basePop[2], n = 16, simParamBee = SP)
  expect_true(all(isDrone(drones, simParamBee = SP)))

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[3], simParamBee = SP)
  colony <- cross(colony, drones = drones, simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100, simParamBee = SP)
  # Expect error if x is not virgin queen or queen
  expect_error(createDrones(colony@workers, simParamBee = SP))

  #Check the caste
  expect_true(all(isDrone(colony@drones, simParamBee = SP)))
  expect_false(all(isDrone(colony@workers, simParamBee = SP)))
  if(packageVersion("AlphaSimR") > package_version("1.5.3")){
    expect_false(all(isDrone(colony@queen@misc$fathers[[1]], simParamBee = SP)))
  }else{
    expect_false(all(isDrone(colony@queen@misc[[1]]$fathers, simParamBee = SP)))
  }

  colony1  <- createColony(basePop[4], simParamBee = SP)
  colony1 <- cross(colony1, drones = drones2, simParamBee = SP)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony, simParamBee = SP), n = 2, nDrones = 15, simParamBee = SP)
  apiary <- createMultiColony(basePop[5:6], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups, simParamBee = SP)

  # Expect error if x = colony and queen isn't present
  colony <- removeQueen(colony, simParamBee = SP)
  expect_error(createDrones(colony, simParamBee = SP))

  #Check s4 class on colony
  expect_s4_class(createDrones(x= colony1, nInd = 5, simParamBee = SP), "Pop")
  expect_error(createDrones(x = colony1, nInd = 0, simParamBee = SP))
  expect_equal(createDrones(x= colony1, nInd = 5, simParamBee = SP)@nInd, 5)

  expect_type(createDrones(x= apiary, nInd = 7, simParamBee = SP), "list")
  expect_length(createDrones(x= apiary, simParamBee = SP), 2)
})

# ---- combineBeeGametes ----

test_that("combineBeeGametes", {
  founderGenomes <- quickHaplo(nInd = 6, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 15, simParamBee = SP)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = drones, simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100, simParamBee = SP)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony, simParamBee = SP), n = 2, nDrones = 15, simParamBee = SP)
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups, simParamBee = SP)

  # Error if used on multicolony or colony class
  expect_error(combineBeeGametes(queen = apiary, drones = dronesGroups, simParamBee = SP))
  expect_error(combineBeeGametes(queen = colony, drones = drones, simParamBee = SP))
  #Error if more than one queen is provided
  expect_error(combineBeeGametes(queen = basePop[5:6], drones = drones, simParamBee = SP))
  # AlphaSimR error - invalid crossPlan
  expect_error(combineBeeGametes(queen = basePop[0], drones = dronesGroups[[1]], simParamBee = SP))
  #check the class
  expect_s4_class(combineBeeGametes(basePop[5], drones = drones, simParamBee = SP), "Pop")
  expect_equal(combineBeeGametes(basePop[5], drones = drones, nProgeny = 5, simParamBee = SP)@nInd, 5)
  suppressWarnings(expect_error(combineBeeGametes(basePop[5], drones = drones, nProgeny = 0, simParamBee = SP)))
})

# ---- pullCastePop ----

test_that("pullCastePop", {
  founderGenomes <- quickHaplo(nInd = 4, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 15, simParamBee = SP)

  # Create Colony and MultiColony class  colony <- createColony(x = basePop[2])
  colony <- createColony(basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = drones, simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100, simParamBee = SP)
  # Colony without workers
  colony1 <- removeWorkers(colony, simParamBee = SP)

  dronesGroups <- pullDroneGroupsFromDCA(getDrones(colony, simParamBee = SP), n = 2, nDrones = 15, simParamBee = SP)
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups, simParamBee = SP)
  apiary <- addWorkers(apiary, nInd = 100, simParamBee = SP)

  apiary1 <- createMultiColony(x = basePop[1], n = 0, simParamBee = SP)
  apiary2 <- createMultiColony(n = 2, simParamBee = SP)
  # test on empty apiary
  expect_type(pullCastePop(apiary1, caste = "queen", nInd = 1, simParamBee = SP)$pulled, "list")
  expect_error(pullCastePop(apiary2, caste = "queen", nInd = 1, simParamBee = SP))
  # Warning- if pulling drones, pulls drones that are not yet mated
  expect_warning(pullCastePop(colony, caste = "drones", nInd = 150, simParamBee = SP))
  suppressWarnings(expect_s4_class(pullCastePop(colony, caste = "drones", simParamBee = SP)$pulled, "Pop"))
  suppressWarnings(expect_s4_class(pullCastePop(colony, caste = "drones", simParamBee = SP)$remnant, "Colony"))

  expect_s4_class(pullCastePop(colony, caste = "workers", simParamBee = SP)$pulled, "Pop")
  expect_s4_class(pullCastePop(colony, caste = "workers", simParamBee = SP)$remnant, "Colony")

  expect_s4_class(pullCastePop(colony, caste = "queen", simParamBee = SP)$pulled, "Pop")
  expect_null(pullCastePop(colony, caste = "virginQueens", simParamBee = SP)$pulled)

  # test on fathers - GitHub  issue made

  # Error is "caste" argument is missing
  expect_error(pullCastePop(colony, simParamBee = SP))
  expect_error(pullCastePop(apiary, simParamBee = SP))

  # type
  expect_type(pullCastePop(colony, caste = "workers", simParamBee = SP), "list")
  expect_type(pullCastePop(colony, caste = "queen", simParamBee = SP), "list")
  suppressWarnings(expect_type(pullCastePop(colony, caste = "drones", simParamBee = SP), "list"))

  expect_type(pullCastePop(apiary, caste = "workers", simParamBee = SP), "list")
  expect_type(pullCastePop(apiary, caste = "queen", simParamBee = SP), "list")
  expect_type(pullCastePop(apiary, caste = "drones", simParamBee = SP), "list")

  # Test whether you pull out more individuals that available
  expect_equal(pullCastePop(colony, caste = "workers", nInd = 10, simParamBee = SP)$pulled@nInd, 10)

})

# ---- cross ----
test_that("cross", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, nInd = 100, simParamBee = SP)
  drones <- createDrones(basePop[1], n = 1000, simParamBee = SP)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 7, nDrones = 15, simParamBee = SP)

  # Create Colony and MultiColony class
  colony <- createColony(basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = dronesGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, nWorkers = 100, nDrones = 100, simParamBee = SP)
  #remove queen
  colony <- removeQueen(colony, simParamBee = SP)
  # If x = colony virgin queens must be present
  expect_error(cross(colony, drones = dronesGroups, simParamBee = SP))
  expect_null(isVirginQueen(colony@virginQueens, simParamBee = SP))

  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  colony1 <- createColony(basePop[5], simParamBee = SP)
  colony2 <- createColony(basePop[6], simParamBee = SP)
  virginQueen <- basePop[7]
  virginQueen2 <- basePop[8]

  # Error if x = pop and are not virgin queens
  expect_error(cross(colony@workers, drones = dronesGroups[[2]], simParamBee = SP))
  expect_null(isVirginQueen(colony@virginQueens, simParamBee = SP))
  # Error if fathers is not a pop
  expect_error(cross(colony1, drones = colony, simParamBee = SP))
  expect_true(isVirginQueen(colony1@virginQueens, simParamBee = SP))
  # Error if fathers are not drones
  expect_error(cross(colony1, drones = colony@workers, simParamBee = SP))
  expect_true(isVirginQueen(colony1@virginQueens, simParamBee = SP))
  # Error if length of fathers doesn't match virginqueens
  expect_error(cross(basePop[3:4], drones = dronesGroups[1], simParamBee = SP))
  expect_true(all(isVirginQueen(basePop[3:4], simParamBee = SP)))
  # If x = colony, queen must not be present
  expect_error(cross(colony, drones = nFathers[1], simParamBee = SP))
  expect_null(isVirginQueen(colony@virginQueens, simParamBee = SP))

  # If x = multicolony, length of fathers must match length of colonies
  expect_error(cross(apiary, drones = dronesGroups[1], simParamBee = SP))
  expect_true(all(isVirginQueensPresent(apiary, simParamBee = SP)))
  # Cannot mate with already-mated drones
  expect_error(cross(colony1, drones = drones, simParamBee = SP))
  expect_true(isVirginQueen(colony1@virginQueens, simParamBee = SP))

  # Must mate prior to S4 class check to change SP
  colony2 <- cross(colony2, drones = dronesGroups[[3]], simParamBee = SP)
  expect_s4_class(colony2,"Colony")
  virginQueen <- cross(virginQueen, drones = dronesGroups[[4]], simParamBee = SP)
  expect_s4_class(virginQueen, "Pop")
  apiary <- cross(apiary, drones = dronesGroups[c(5,6)], simParamBee = SP)
  expect_s4_class(apiary, "MultiColony")

  # Error when mating a mated queen
  expect_error(cross(colony2, drones = dronesGroups[7], simParamBee = SP))

  # Message if fathers == 0 "Mating failed"
  expect_error(cross(virginQueen2, drones= selectInd(colony@drones, nInd = 0, use = "rand", simParam = SP), simParamBee = SP))
  #expect_message(cross(virginQueen2, drones= selectInd(colony@drones,nInd = 0, use = "rand", simParam = SP), checkCross = "warning", simParamBee = SP))
})

# ---- setQueensYearOfBirth ----
test_that("setQueensYearOfBirth", {
  founderGenomes <- quickHaplo(nInd = 7, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, nInd = 100, simParamBee = SP)
  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(x = colony, drones = dronesGroups[[1]], simParamBee = SP)
  colony <- buildUp(colony, simParamBee = SP)
  # Error if x = pop, and not a vq or q
  expect_error(setQueensYearOfBirth(colony@workers, simParamBee = SP))
  expect_error(setQueensYearOfBirth(colony@drones, simParamBee = SP))

  colony <- removeQueen(colony, simParamBee = SP)
  # Error if x = colony and no queen is present
  expect_error(setQueensYearOfBirth(colony, simParamBee = SP))

  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups[c(2, 3)], simParamBee = SP)

  colony1 <- createColony(x = basePop[5], simParamBee = SP)
  colony1 <- cross(colony1, drones = dronesGroups[[4]], simParamBee = SP)
  queen1 <- getQueen(colony1, simParamBee = SP)

  expect_s4_class(setQueensYearOfBirth(queen1, year = 2022, simParamBee = SP), "Pop")
  expect_s4_class(setQueensYearOfBirth(colony1, year = 2022, simParamBee = SP), "Colony")
  expect_s4_class(setQueensYearOfBirth(apiary, year = 2022, simParamBee = SP), "MultiColony")
})

# ---- createDCA ----
test_that("createDCA", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = dronesGroups[[1]], simParamBee = SP)
  apiary <- createMultiColony(basePop[3:4], n = 2, simParamBee = SP)
  apiary <- cross(apiary, drones = dronesGroups[c(2, 3)], simParamBee = SP)
  # Warning if x = colony/multicolony and no drones available
  expect_warning(createDCA(colony, simParamBee = SP))
  expect_warning(createDCA(apiary, simParamBee = SP))
  # Error if another caste is used
  expect_error(createDCA(basePop[6], simParamBee = SP))

  colony1 <- createColony(x = basePop[5], simParamBee = SP)
  colony1 <- addDrones(colony, nInd = 100, simParamBee = SP)

  colony2 <- createColony(x = basePop[6], simParamBee = SP)
  colony2 <- cross(colony2, drones = selectInd(colony1@drones, nInd = 1, use = "rand", simParam = SP), simParamBee = SP)

  expect_s4_class(createDCA(colony1, nInd = 10, simParamBee = SP), "Pop")
  expect_equal(createDCA(colony1, nInd =10, simParamBee = SP)@nInd, 10)

  #empty apiary
  apiary1 <- createMultiColony(n = 3, simParamBee = SP)
  expect_error(createDCA(apiary1, simParamBee = SP))
})

# ---- pullDroneGroupsFromDCA ----
test_that("pullDroneGroupsFromDCA", {
  founderGenomes <- quickHaplo(nInd = 8, nChr = 1, segSites = 100)
  SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
  SP$nThreads = 1L
  basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

  drones <- createDrones(x = basePop[1], nInd = 1000, simParamBee = SP)
  expect_length(pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP), 10)
  dronesGroups <- pullDroneGroupsFromDCA(drones, n = 10, nDrones = nFathersPoisson, simParamBee = SP)

  # Create a Colony and a MultiColony class
  colony <- createColony(x = basePop[2], simParamBee = SP)
  colony <- cross(colony, drones = dronesGroups[[1]], simParamBee = SP)
  colony <- addDrones(colony, nInd = 100, simParamBee = SP)

  DCA <- createDCA(colony, nInd = 80, simParamBee = SP)
  # Error, DCA must be a Pop
  expect_error(pullDroneGroupsFromDCA(colony, simParamBee = SP))
  # Error, n must be provided
  expect_error(pullDroneGroupsFromDCA(DCA, simParamBee = SP))

  expect_type(pullDroneGroupsFromDCA(DCA, n = 5, nDrones = 8, simParamBee = SP), "list")
  expect_s4_class(pullDroneGroupsFromDCA(DCA, n = 5, nDrones = 8, simParamBee = SP)[[1]], "Pop")
  expect_true(all(isDrone(pullDroneGroupsFromDCA(DCA, n = 1, nDrones = 70, simParamBee = SP)[[1]], simParamBee = SP)))

  suppressWarnings(DCA2 <- createDCA(colony, nInd = 4, simParamBee = SP))
  # Error, if nInd in DCA is smaller than nFathers
  expect_error(pullDroneGroupsFromDCA(DCA2, n =10, nDrones = 20, simParamBee = SP))
})

test_that("combineBeeGametes", {
   founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   queen <- basePop[1]
   drones <- createDrones(x = basePop[2], nInd = 5, simParamBee = SP)
   workers <- SIMplyBee:::combineBeeGametes(queen, drones, nProgeny = 4, simParamBee = SP)

   expect_equal(drones@ploidy, 2)
   expect_equal(nInd(drones), 5)
   expect_equal(workers@ploidy, 2)

})

test_that("combineBeeGametesHaploidDiploid", {
   founderGenomes <- quickHaplo(nInd = 3, nChr = 1, segSites = 100)
   SP <- SimParamBee$new(founderGenomes, csdChr = NULL)
   SP$nThreads = 1L
   basePop <- createVirginQueens(founderGenomes, simParamBee = SP)

   queen <- basePop[1]
   drones <- reduceGenome(
     pop = basePop[2], nProgeny = 5, keepParents = FALSE,
     simRecomb = TRUE, simParam = SP
   )
  expect_equal(nInd(drones), 5)
  expect_equal(drones@ploidy, 1)
})

