## init testthat
library('testthat')

source("cachematrix.R")

test_that("matrix set/get", {
  testMatrix = matrix(
    c(1,2,3,4,5,6),
    nrow=2,
    ncol=3
    )
  
  expect_that(testMatrix, is_a("matrix") )
  
  cachedMatrix = makeCacheMatrix(testMatrix)
  
  expect_that(cachedMatrix, is_a("list"))
  expect_that(cachedMatrix$set, is_a("function"))
  expect_that(cachedMatrix$get, is_a("function"))
  expect_that(cachedMatrix$get(), is_a("matrix"))
  expect_that(cachedMatrix$get()[1, 2], is_a("numeric"))
  expect_equal(cachedMatrix$get()[1, 2], 3)
  #print(testMatrix)
  expect_equal(cachedMatrix$get(), testMatrix)
  expect_equivalent(cachedMatrix$get(), testMatrix)
  
  badMatrix = makeCacheMatrix()
  expect_that(badMatrix$get(), is_a("matrix"))
  expect_that(badMatrix$get()[1, 1], is_a("logical"))
  expect_equal(badMatrix$get()[1, 1], NA)
  expect_equal(length(badMatrix$get()[1,]), 1)
  expect_equal(length(badMatrix$get()[,1]), 1)
  #print(badMatrix$get())
  
  badMatrix = makeCacheMatrix(7)
  expect_that(badMatrix$get(), is_a("numeric"))
  expect_that(badMatrix$get()[1], is_a("numeric"))
  expect_equal(badMatrix$get()[1], 7)
  
  badMatrix$set(8)
  expect_that(badMatrix$get(), is_a("numeric"))
  expect_that(badMatrix$get()[1], is_a("numeric"))
  expect_equal(badMatrix$get()[1], 8)
  
  badMatrix$set(testMatrix)
  goodMatrix <- badMatrix
  expect_that(goodMatrix, is_a("list"))
  expect_that(goodMatrix$set, is_a("function"))
  expect_that(goodMatrix$get, is_a("function"))
  expect_that(goodMatrix$get(), is_a("matrix"))
  expect_that(goodMatrix$get()[1, 2], is_a("numeric"))
  expect_equal(goodMatrix$get()[1, 2], 3)
  #print(testMatrix)
  expect_equal(goodMatrix$get(), testMatrix)
  expect_equivalent(goodMatrix$get(), testMatrix)

  #print(goodMatrix)
})

test_that("matrix setInvMatrix/getInvMatrix", {
  
  simpleMatrix <- matrix(c(1,0,0,1),
                      ncol=2,
                      nrow=2)
  invMatrix <- solve(simpleMatrix)
  
  smartMatrix <- makeCacheMatrix(simpleMatrix)
  smartMatrix$setInvMatrix(invMatrix)
  
  expect_that(smartMatrix$getInvMatrix(), is_a("matrix"))
  expect_equal(smartMatrix$getInvMatrix(), invMatrix)
})

test_that("cacheSolve returns propper inverses", {
  simpleMatrix <- matrix(c(1,2,3,4),
                         ncol=2,
                         nrow=2)
  invMatrix <- solve(simpleMatrix)
  
  smartMatrix <- makeCacheMatrix(simpleMatrix)
#  print(simpleMatrix)
#  print(invMatrix)
#  print(smartMatrix)
  expect_equal(cacheSolve(smartMatrix), invMatrix)
  size <- 1024

  hugeMatrix <- matrix(0, nrow=size, ncol=size)
  for(i in 1:size)
  {
    hugeMatrix[i,i] <- i
  }
  
  smartHugeMatrix <- makeCacheMatrix(hugeMatrix)
  expect_equal(cacheSolve(smartHugeMatrix), solve(hugeMatrix))

})

test_that("cacheSolve optimizes speed", {
  size <- 1024*2
  hugeMatrix <- matrix(0, nrow=size, ncol=size)
  for(i in 1:size)
  {
    hugeMatrix[i,i] <- i
  }
  
  time_1_1 <- system.time(solve(hugeMatrix))
  time_1_2 <- system.time(solve(hugeMatrix))
  expect_equal(time_1_1, time_1_1)
    
  smartHugeMatrix <- makeCacheMatrix(hugeMatrix)
  time_2_1 <- system.time(cacheSolve(smartHugeMatrix))
  time_2_2 <- system.time(cacheSolve(smartHugeMatrix))
 
  expect_true(time_2_1["user.self"] > time_2_2["user.self"])
})