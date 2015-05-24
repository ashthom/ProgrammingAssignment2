## Put comments here that give an overall description of what your
## functions do

## return a list which contains functions to set or get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## calculates the inverse of the matrix. If the inverse is already in cache
## then return it else compute it using 'solve'.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
