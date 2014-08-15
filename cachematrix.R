## The following functions implement the caching of a matrix with its inverse
## c <- makeCacheMatrix() makes a cache
## c$get() returns the matrix stored
## c$set(matrix) stores the new matrix
## cacheSolve(cache) solves or only accesses the matrix

## Creates a cache object with its value initialized to the pass matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinv = setinverse, getinv = getinverse)
}
## Calculates, stores, and returns the inverse of the cached matrix if it doesnt
## exist otherwise returns the inverse if it already exists.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  xmatrix <- x$get()
  i <- solve(xmatrix, ...)
  x$setinverse(i)
  return(i)
}
