## These two functions work as a pair to cache the inverse of a matrix

## This function creates a matrix called 'x' and a NULL placeholder for its
## inverse called 'inv'. It also creates these four functions:
##     * set - assings the values of matrix 'x' to 'y' and NULL to 'inv'
##     * get - retrieves the matrix 'x'
##     * setinverse - assigns the value of a given matrix to 'inv'
##     * getinverse - retrieves the value of 'inv'
## Finally, makeCacheMatrix returns a list object containing these functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(new_inv) inv <<- new_inv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function attempts to retrieve a cached matrix that is the inverse of 'x'
## from the object created by makeCacheMatrix. If 'inv' (the placeholder for the
## inverse) is not NULL, then the function returns 'inv'. If 'inv' is NULL, then
## the function retrieves the value of 'x' from the makeCacheMatrix object,
## calculates its inverse, resets the value of 'inv' from the makeCacheMatrix
## object, and returns 'inv'.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
