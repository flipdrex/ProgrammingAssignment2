
## Finding the inverse of a matrix is a costly computation. These functions cache the
## inverse of a matrix.

## This function creates a special matrix that can cashe its inverse.
## The function creates a list of functions to
## 1. Set the value of the Matrix
## 2. Get the value of the Matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y){
    y<<-x
    inv<<-NULL
  }
  getmat<-function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## This funtion looks for the value of the cached inverse. If the value is not cached,
## the function finds the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmat()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
