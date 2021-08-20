## Put comments here that give an overall description of what your
## functions do

# function to cache matrix. Returns get and set for matrix
makeCacheMatrix <- function(x = matrix()) 
{

    p <- NULL
    setting <- function(q) 
    {
      w <<- q
      p <<- NULL
    }
    getting <- function() w
    makeInverse <- function(inverse) p <<- inverse
    retrieveInverse <- function() p
    list(setting = setting, getting = getting, p
         akeInverse = retrieveInverse, retrieveInverse = retrieveInverse)
  
}

# Calculates inverse of matrix or retrieves
# a previously calculated inverse from cache
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(w, ...)
{
    cacheSolve <- function(w, ...)
    {
        p <- w$retrieveInverse()
        if(!is.null(p)) 
      {
        return(p)
      }
        data <- w$getting()
        p <- solve(data) %*% data
        w$makeInverse(p)
      p
    }
}

# conclude

