## This pair of functions calculates the inverse of a given matrix and then
## stores this value in the cache for subsequent use.

## makeCacheMatrix stores a matrix and its inverse. It also creates a list of 
## functions which can then be used by cacheSolve to retrieve the matrix,
## and both retrieve and/or set the value of the matrix's inverse.

makeCacheMatrix <- function(matriz = matrix()) {
    inv <- NULL
    set <- function(y) {
        matriz <<- y
        inv <<- NULL
    }
    get <- function() {matriz} 
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {inv}
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function does the inverse calculation, and sets the value of this
## inverse. However, if the inverse is already stored in the cache, it instead
## returns the value from the cache.

cacheSolve <- function(matriz, ...) {
        ## Return a matrix that is the inverse of 'x' if it is stored in the cache
    inv <- matriz$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ##If not in the cache, then calculate it
    datos <- matriz$get()
    inv <- solve(datos, ...)
    matriz$setinverse(inv)
    inv
}
