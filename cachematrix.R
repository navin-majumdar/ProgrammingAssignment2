## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## x: a square invertible matrix input
  ## returns: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##this list is used as the input to cacheSolve()
  inv <- NULL
  
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment
	x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of original matrix 'x', also input to makeCacheMatrix()
  ## x: output of makeCacheMatrix()
  inv <- x$getInverse()
  # if the inverse has already been calculated
  if (!is.null(inv)) {
    # get it from the cache and skips the computation
	message("getting cached data")
    return(inv)
  }
  # otherwise, calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # sets the value of the inverse in the cache via the setinv function
  x$setInverse(inv)
  
  return(inv)
}
