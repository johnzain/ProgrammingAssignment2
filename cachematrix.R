## These are two functions which create a cached inverse of a matrix 
## and retrive the cached matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
      x <<- y
      inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse_matrix <- function(inverse) inverse_matrix <- inverse
    getinverse_matrix <- function() inverse_matrix
    list(set = set, get = get, setinverse_matrix = setinverse_matrix, getinverse_matrix = getinverse_matrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse_matrix ()
    if(!is.null(inverse_matrix)){
      message("getting cached data")
      return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data,...)
    x$setinverse_matrix(inverse_matrix)
    inverse_matrix
}
