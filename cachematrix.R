## makeCacheMatrix makes a "special matrix" that can store the inverse.
## cacheSolve returns the cached inverse if available, otherwise computes the inverse.

## makeCacheMatrix returns a list of 4 components:
## set: can set the matrix
## get: returns the matrix
## setinverse: caches the matrix inverse
## getinverse: returns the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve first looks for cached inverse, and return it if available
## if cached inverse is not available, calculates the inverse and return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}