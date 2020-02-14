## This function creates a special "matrix" object that can cache its 
## inverse. The special "matrix" is a list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(invx) inv <<- invx
  
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function calculates the inverse of the special "matrix" created
## with the above makeCacheMatrix function. However, it first checks 
## to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}
