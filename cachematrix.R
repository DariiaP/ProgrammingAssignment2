## Put comments here that give an overall description of what your
## functions do

## Funcion for wrap matrix with cache and cache acces functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) { # set value & reset cached value delegate function
    x <<- y
    inv <<- NULL
  }
  
  get <- function() { # get value delegate function
    x
  }
  
  setinv <- function(i) { # set cached value delegate function
    inv <<- i
  }
  
  getinv <- function() { # get cached value delegate function
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Matrix solve fuction with chaching

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # get cached value; can be empty
  
  if(!is.null(inv)) { # check: is cache not empty  
    message("used cached data")
  } else { # if empty, solve matrix & set cache
    message("solving and set cache")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }
  
  inv # return final value
}

