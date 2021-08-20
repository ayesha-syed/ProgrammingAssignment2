## Put comments here that give an overall description of what your
## functions do

# function to cache matrix. Returns get and set for matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  makeInverse <- function(inverse) m <<- inverse
  retrieveInverse <- function() m
  list(set = set, get = get, makeInverse = retrieveInverse, retrieveInverse = retrieveInverse)
  
}

# Calculates inverse of matrix or retrieves
# a previously calculated inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$retrieveInverse()
    if(!is.null(m)) {
      return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$makeInverse(m)
    m}
}

# conclude

