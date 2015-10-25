## These functions store a matrix, compute its inverse
## and cache it to avoid computing it repeatedly

## stores a matrix and its cached inverse
## returns a list of functions for accessing them
makeCacheMatrix <- function(x = matrix()) {
    # cached matrix inverse
    i <- NULL
    # get the matrix
    get <- function() x
    # set the matrix, resetting the cached inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # get and set the inverse
    getinverse <- function() i
    setinverse <- function(inverse) i <<- inverse
    # return the accessor functions
    list(get = get, set = set,
         getinverse = getinverse,
         setinverse = setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        # the inverse is cached, return it
        return(i);
    }
    # compute the inverse
    m <- x$get()
    i <- solve(m)
    # cache the result and return it
    x$setinverse(i)
    return(i)
}
