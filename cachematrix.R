## Coursera rprog-011 Assignment 2
### Nelson To
### Feb 22, 2015

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL #sets Inv to null
  y <- NULL #sets y to null
  
  set <- function(y) { #sets values to x and inverse
    x <<- y # caches the matrix
    Inv <<- NULL # caches the Inverse
  }
  
  get <- function() x
  
  setInv <- function(Inverse) Inv <<- Inverse
  
  getInv <- function() Inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv) #creates the special matrix containing a list of the four functions: set, get, setInv, getInv
}

# cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. If the matrix is unchanged and the inverse has already been calculated, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix()) {
  # Return a matrix that is the inverse of 'x'
  
  Inv <- x$getInv() 
  
  if(!is.null(Inv)) { # If the inverse is null, this function will be skipped. If it is not null, it will continue to check if the inverse is identical to the current inverse.
    if(identical(x$get(),Inv)){
      message("cached data has been calculated before, getting cached data")
      return(Inv)
    } else {
      message("matrix is new, calculating inverse")
    }
  }
  
  data <- x$get()
  Inv <- solve(data)
  x$setInv(Inv)
  message("here is your inverse")
  Inv
}
