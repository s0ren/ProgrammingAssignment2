## I asume that the function solve() returns an equivelent to an inverse matrix, 
## as described in John Myles White's article "Quick Review of Matrix Algebra in R": http://www.r-bloggers.com/quick-review-of-matrix-algebra-in-r/

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # set internat representation of inverse matrix to NULL
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}
