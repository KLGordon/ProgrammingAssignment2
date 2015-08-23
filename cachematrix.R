## Assignment 2 - cache and return of matrix input and its inverse

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL 
        }
        get <- function() x 
        setinv <- function(solve) m <<- solve(x)
        getinv <- function() m 
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## computes the inverse of the 'makeCacheMatrix' matrix and retrieves 
## the inverse from the cache if it has already been calculated


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
