## Put comments here that give an overall description of what your
## functions do

## Funcion for wrap matrix with cache and cache acces functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Matrix solve fuction with chaching

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("used cached data")
  } else {
    message("solving and set cache")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }
  
  inv
}

