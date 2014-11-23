# You can create a special matrix object that stores a matrix and caches its inverse.
# Keep in mind that it assumes that the matrix given is always INVERTIBLE.


# Description
# This function creates a special matrix object that stores a matrix and caches
# its inverse. It should be used with the cacheSolve function described below 
# as it might cause internal data consistency when used independently.
#
# Usage
# makeCacheMatrix(x)
#
# Arguments
# x: an invertible matrix
#
# Examples
# source('cachematrix.R') 
# m <- rbind(c(1, 2), c(3, 4))
# makeCacheMatrix(m)
makeCacheMatrix <- function(x = matrix()) {
    inv.x <- NULL

    set <- function(y) {
        x <<- y
        inv.x <<- NULL
    }
    get <- function() x

    setInverse <- function(inverse) inv.x <<- inverse
    getInverse <- function() inv.x

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Description
# This function returns the inverse of an input matrix returned by makeCacheMatrix.
# If the inverse has NOT been requested before then it will calculate the inverse
# with the function 'solve' in R and cache the inverse. Otherwise, it will
# retrieve the inverse from the cache.
#
# Usage
# cacheSolve(x, ...)
#
# Arguments
# x: an object returned by makeCacheMatrix.
# ...: further arguments passed to the solve() function in R.
#
# Return
# the inverse matrix of the input x
#
# Examples
# source('cachematrix.R') 
# m <- rbind(c(1, 2), c(3, 4))
# cache.mat <- makeCacheMatrix(m)
# cacheSolve(cache.mat) # calculates and returns the inverse matrix of cache.mat
# cacheSolve(cache.mat) # retrieves and returns the cached inverse matrix of cache.mat 
cacheSolve <- function(x, ...) {
    inv.x <- x$getInverse()
    if(!is.null(inv.x)) {
        message("getting cached data")
        return(inv.x)
    }
    x$setInverse(solve(x$get(), ...))
    x$getInverse()
}
