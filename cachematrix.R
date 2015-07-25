## The following two functions will cache and compute the inverse of a matrix.
## Example:
## > c=rbind(c(1, -1/4), c(-1/4, 1))
## > invC=cacheSolve(makeCacheMatrix(c))
## > invC
##        [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667


## This function creates an object that can be used to cache 
## the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() return(x)
    set_inverse <- function(inverse) inverse_matrix <<- inverse
    get_inverse <- function() return(inverse_matrix)
    return(list(set = set, get = get, set_inverse = set_inverse, 
                get_inverse = get_inverse))
}


## This function computes the inverse of the matrix returned by 
## "makeCacheMatrix" function. If the inverse is already calculated, this 
## function will retrieve the inverse stored in the cache. 

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$get_inverse()
    if (!is.null(inverse_matrix)) {
        message(" getting cached data")
        return (inverse_matrix)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    ## Return a matrix that is the inverse of 'x'
}
