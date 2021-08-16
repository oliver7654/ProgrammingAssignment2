## Matrix inversion is a costly operation.
## A pair of functions 'makeCacheMatrix' and 'cacheSolve' allows
## to cache the result of the inversion.

## Creates a matrix object that can cache the result of an inversion
makeCacheMatrix <- function(a = matrix()) {
  a_inv <- NULL
  set <- function(b) {
    a <<- b
    a_inv <<- NULL
  }
  get <- function() a
  setinv <- function(inv) a_inv <<- inv
  getinv <- function() a_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of a 'cache matrix'.
## A previously computed result is reused if available.
cacheSolve <- function(a, ...) {
  inv <- a$getinv()
  if(!is.null(inv)) {
    print('Using cached data.')
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setinv(inv)
  inv
}
