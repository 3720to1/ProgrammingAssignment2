## makeCacheMatrix accepts a matrix and creates a special matrix used
## by cacheSolve. cacheSolve accepts an invertible matrix and calculates 
## its inverse, returning a cached version of the inverse if available

## creates a special "matrix" from the matrix x, which is really a list
## containing functions to set/get the value of the matrix and set/get
## the value of the inverse, thus, it can hold and allow access to
## both the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    i <<- inverse  
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## solves the inverse of the invertible matrix x, given that x was 
## created by makeCacheMatrix, returns previously cached inverses 
## when available
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached result")
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
