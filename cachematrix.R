## This R script defines a pair of functions that stores the inverse of a matrix in a cache.

## makeCacheMatrix caches the inverse of a new matrix object.

makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    get <- function() matrix
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, cacheSolve returns the cached inverse.

cacheSolve <- function(special_matrix, ...) {
    inverse <- special_matrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data...")
        return(inverse)
    }
    data <- special_matrix$get()
    inverse <- solve(data, ...)
    special_matrix$set_inverse(inverse)
    inverse
}
