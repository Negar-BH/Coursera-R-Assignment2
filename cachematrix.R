## Matrix inversion is a costly computation. 
## Caching the inverse of a matrix rather than compute it repeatedly 
## helps us to reduce the time of executing inversion. Following functions 
## are written to cache the inverse of a matrix.

## makeCacheMatrix function creates a special matrix and caches its inverse.
## this function contains the functions for: 
##       1.setting the value of the matrix
##       2.getting the value of the matrix
##       3.setting the value of matrix inversion
##       4.getting the value of matrix inversion


makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) r <<- inverse
        getInverse <- function() r
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse )
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getInverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setInverse(r)
        r
}
