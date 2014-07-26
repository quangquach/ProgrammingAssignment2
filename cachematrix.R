## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" which is containing following functions
# get: get the matrix value
# set: set the matrix value
# getinverse: get the inverse of matrix
# setinverse: set the inverse of matrix
# makeCahceMatrix takes the usual matrix as the argument

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# This function calculate the inverse of the matrix
# It take the special matrix (created by makeCacheMatrix) as the argument.
# The function check if the matrix has cached inverse.
# If yes, it returns the cached data.
# Otherwise, it calculates the inverse of the matrix and caches it into the matrix, then return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }

  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
