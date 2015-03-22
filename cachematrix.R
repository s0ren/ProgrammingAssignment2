## I asume that the function solve() returns an equivelent to an inverse matrix,
## as described in John Myles White's article 
## "Quick Review of Matrix Algebra in R": http://www.r-bloggers.com/quick-review-of-matrix-algebra-in-r/

#### Please see the test (somewhat exaustive), for the functions in this file. See test_cachematrix.R

## This code consists of two main functions makeCacheMatrix() and cacheSolve(). 
##
## Function makeCacheMatrix() serves as a constructor or factory, and returns a list of
## getter and setter methods. The list also serves as a container for the matrix it
## self, and the inverse matrix - via the internal namespace of makeCachedMatrix().
##
## Function cacheSolve() returns ether the cached inverse matrix, or request the
## solve(ed). cacheSolve() also has responsebility for storing the calculated
## inverse matrix, by calling setInvMatrix() on the "smart" matrix structure.

### makeCachedMatrix()
## Parameters: 
##    x, a matrix (should be square, but not required), defaults to an empty matrix
## Returns
##    a list of four functions

makeCacheMatrix <- function(x = matrix()) {
  # set internal representation of inverse matrix to NULL
  im <- NULL
  
  ## set (Matrix)
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## get (Matrix)
  get <- function() x
  
  ## setInvMatrix
  setInvMatrix <- function(invMatrix) im <<- invMatrix
  
  ## getInvMatrix 
  getInvMatrix <- function() im
  
  ## return a list of methods
  list(set=set,
       get=get,
       setInvMatrix=setInvMatrix,
       getInvMatrix=getInvMatrix)
}


### cacheSolved() 
## Paramters
##    x, a cachedMatrix
## Returns
##    the inverse of the matrix in x

cacheSolve <- function(x, ...) {
  # grab whatever is in the cachedMatrix
  m <- x$getInvMatrix()
  # check if inverse matrix is already calculated and storred
  if(!is.null(m)) {
    # if inverse matrix is storred, return it
    message("getting cached data")
    return(m)
  }
  else
  {
    # inverse matrix IS NOT storred
    # get the matrix
    data <- x$get()
    # solve inverse
    m <- solve(data, ...)
    # store inverse matrix in cacheMatrix namespace with setInvMatrix()
    x$setInvMatrix(m)
    # return inverse matrix
    m
  }
}
