## With makeCacheMatrix Im creating a matrix object that can cache its own inverse,
## based on the given example on how to cache a mean

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invmat <<- inverse
        getInverse <- function() invmat
        list(set = set,get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve finds the inverse of the matrix objetc created before, and if the
## inverse has alredy been calculated, then it gets it from the cache without
## calculating it again


cacheSolve <- function(x, ...) {
        invmat <- x$getInverse()
        if (!is.null(invmat)) {
                message("hold on.. getting data from cache")
                return(invmat)
        }
        mat <- x$get()
        invmat <- solve(mat, ...)
        x$setInverse(invmat)
        invmat
}