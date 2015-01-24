## Caching the inverse of a matrix.

## Creates special matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  # return list of functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function for caching inverse of matrix if matrix is not changed, else compute inverse.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  #return cached inverse
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  
  #return inverse
  inverse
}
