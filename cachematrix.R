## R Programming Assignment 2 2015-01-25
## Matrix Inverse
## Calculates the inverse of a function.  Tests first to see
## if the inverse matrix already exists, to speed calculations.

# Sets the value of the matrix, gets the value of the matrix.
# Sets the value of the inverse matrix; gets the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Checks to see if the matrix inverse has already been cacluated
## If it hasn't, calculates it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
