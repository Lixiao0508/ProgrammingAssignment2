makeCacheMatrix <- function(x = numeric()) {
  
  #set initial value of cache to NULL if nothing is cached
  cache <- NULL
  
  # set and store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  
  # return the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in the cache
  matrixdata <- y$getMatrix()
  inverse <- solve(matrixdata)
  y$cacheInverse(inverse)
  
  # return the value of inverse of matrix
  inverse
}
