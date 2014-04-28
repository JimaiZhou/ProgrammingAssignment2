## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list of four functions that operate on the input matrix
makeCacheMatrix <- function(x = matrix()) {
        # sets the inverse(cache) to be NULL
        inv <- NULL
        # the set function can be used the change the matrix associated with this function
        set <- function(y) {
                x <<- y
                # reset the computed inverse to NULL(clearing the cache)
                inv <<- NULL
        }
        # the get function retrives the matrix x
        get <- function() x
        # the setinv function takes an input and loads it into the cache
        setinv <- function(solve) inv <<- solve
        # the getinv function retrieves the cached inverse
        getinv <- function() inv
        # returns the list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
# this function computes the the inverse only if there is none in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # load the cahce
        y <- x$getinv()
        # if cache is none empty, return the cached data
        if(!is.null(y)) {
                message("getting cached data")
                return(y)
        }
        # otherwise, retrieves the matrix x, compute the inverse, load it into the cache, and return it
        data <- x$get()
        y <- solve(data, ...)
        x$setinv(y)
        y
}
