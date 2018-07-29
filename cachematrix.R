## The idea of this assignment is to create 2 functions makeCacheMatrix and cacheSolve
## These functions will be used to create and compute inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x has to be a square invertible matrix
  ## Here we are creating list having functions to set/Get Matrix and set/Get Inverse
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Here x is the output and returns inverse of the matrix that was passed as input
  
  inv <- x$getInverse()
  
  ## if the inverse is already available, get from cache
  if (!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  
  ## Else calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  
  ## Set the value of the inverse in the cache
  x$setInverse(inv)
  
  return(inv)
}
