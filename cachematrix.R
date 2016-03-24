# Coursera Programming Assignment : Matrix Inversion
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.


# The function makeCacheMatrix creates an inversible matrix.

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invs <<- inverse
  getInverse <- function() invs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The function cacheSolve computes the inverse of the matrix, if it is not 
# already stored in the cache, then it retrieves the inverse matrix from the cache. 

cacheSolve <- function(x, ...) {

  invs <- x$getInverse()
  if (!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setInverse(invs)
  invs
}