## This pair of functions calculates the inverse of a given matrix and then
## stores this value in the cache for subsequent use.

## makeCacheMatrix stores a matrix and its inverse. It also creates a list of 
## functions which can then be used by cacheSolve to retrieve the matrix,
## and both retrieve and/or set the value of the matrix's inverse.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    
    ##This function allows the user to change the matrix without using makeCacheMatrix
    ##again. It also erases the cached value of the inverse.
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() {mat} 
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix created by makeCacheMatrix,
## and sets such value to the parent environment using setinverse(). However,
## if the inverse is already stored in the cache, it instead returns the value
## from the cache.

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x' if it is stored in the cache
    inv <- mat$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ##If not in the cache, then calculate it
    dat <- mat$get()
    inv <- solve(dat, ...)
    mat$setinverse(inv)
    inv
}
