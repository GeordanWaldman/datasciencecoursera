# This function will inverse a matrix.
# makeCacheMatrix creates a list for a function that
# sets and gets that values of the matrix and it's inverse.
# i is equal to inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of a matrix returned by
# makeCacheMatrix. If the inverse has been calculated, then
# this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data.")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}