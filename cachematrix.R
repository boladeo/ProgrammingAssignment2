## The two functions below help to create and provide functionality for
## a special matrix object whose inverse can be computed and cached across
## multiple calls helping to provide performance boost.
##
## Usage  - 1. Call makeCacheMatrix to set up the special matrix object passing it 
##             an invertible matrix
##          2. Call cacheSolve on the special matrix object to return the inverse of 
##             your invertible matrix. It is assumed that the initialized matrix is 
##             invertible. 
##
## Example: source("cachematrix.R")
##          m1 <- makeCacheMatrix( matrix(c(1,2,3,0,4,5,1,0,6), 3,3,TRUE))
##          cacheSolve(m1) #first call computes the inverse and caches the result
##          cacheSolve(m1) #subsequent calls return the inverse from cache


## The function below takes a matrix and returns a wrapper matrix object
## with a list of functions and variable that help to cache the computed matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolveMatrix <- function(solve) m <<- solve
  getSolveMatrix <- function() m
  list(set = set, get = get,
       setSolveMatrix = setSolveMatrix,
       getSolveMatrix = getSolveMatrix)
}


## This function returns the cached inverse of the matrix if it has already been computed
## before and cached, otherwise it computes the inverse, caches it and returns the inverse 
## to the caller. This provides performance boost especially if there is the need to compute
## the inverse multiple times.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolveMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolveMatrix(m)
  m
}
