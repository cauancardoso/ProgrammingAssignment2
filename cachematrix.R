## This function caches the inverse of a matrix X, if X is a square
## invertible matrix. To use this function correctly, first, you 
## need to assign a square invertible matrix to an object; then, 
## pass it to the the makeCacheMatrix() function. This will create 
## an R object that stores a matrix and its inverse. Next, pass 
## your object to the cacheSolve() function. This will retrieve 
## the the inverse from the cached value that is stored in the 
## makeCacheMatrix() object's environment.

## makeCacheMatrix: This function creates a special "matrix" object,
## which is a list containing a function to set the value of the
## matrix; get the value of the matrix; set the value of the inverse;
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
 
}
