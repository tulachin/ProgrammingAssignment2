## This pair of functions calculates the inverse of a given matrix and then
## stores this value in the cache for subsequent use.

## makeCacheMatrix stores a matrix and its inverse. It also creates a list of 
## functions which can then be used by cacheSolve to retrieve the matrix,
## and both retrieve and/or set the value of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function does the inverse calculation, and sets the value of this
## inverse. However, if the inverse is already stored in the cache, it instead
## returns the value from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
