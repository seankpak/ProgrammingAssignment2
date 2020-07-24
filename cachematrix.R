## These functions will create a special matrix object that can cache
## its inverse. Then it will compute the inverse of the special
## matrix returned. If the inverse has already been calculated 
## then the cachsolve should retrieve the inverse from the cache

## This function will create a special matrix object that can cache 
## its inverse

makeCacheMatrix <- function(x = martrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse  <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function will compute the inverse of the special matrix
## returned by makeCacheMatrix above. If inverse has already been
## calculated, then the cachesolve will retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
}


