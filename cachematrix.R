## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          
  iverse <- NULL
  set <- function(z) {
    y <<- z
    iverse <<- NULL
  }
  get <- function() y
  sInverse <- function(inverse) iverse <<- inverse
  gInverse <- function() iverse
  list(set = set, get = get, sInverse = sInverse,  gInverse = gInverse)

}


## This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iverse <- x$gInverse()
  if (!is.null(iverse)) {
    message("Getting Cached Data")
    return(iverse)
  }
  matrix <- x$get()
  iverse <- solve(matrix, ...)
  x$sInverse(iverse)
  iverse
}
