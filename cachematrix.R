## Functions to Cache the Inverse of a Matrix
## Matrix inversion can be a costly computation, so caching the inverse of a matrix
## instead of repeatedly computing it is useful. The following functions create an
## object to store a matrix and cache its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function()x
    setInv <- function(inverse) mat_inv <<- inverse
    getInv <- function() mat_inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    mat_inv <- x$getInv()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    mat <- x$get()
    mat_inv <- solve(mat, ...)
    x$setInv(mat_inv)
    mat_inv
        ## Return a matrix that is the inverse of 'x'
}
