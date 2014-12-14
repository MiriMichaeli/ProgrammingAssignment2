## Function cacheSolve computes the inverse matrix of an input matrix given to Function makeCacheMatrix. 
## Function makeCacheMatrix creates a List that contain 4 set/get functions to deal with the calculations.
## makeCacheMatrix saves the matrix and its inverse in the cache for multiple uses.
## In case the matrix is replaced, or in the first calculation, the inverse matrix is computed,
## and saved in the cache, by the cacheSolve function. 

## This function creates a special "matrix" object, which is really a list 
## containing a function to get and set a matrix and its inverse, and cache it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve return the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
