## This pair of functions can be used to cache the inverse of a matix.
## The benefit of caching the inverse is to prevent having to repeat
## this potentially costly computation.

## The 'makeCacheMatrix' function creates a special "matrix" object that
## can cache its inverse. This function returns a list of functions that 
## set the value of the matrix, get the value of the matrix, set the
## value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The 'cacheSolve' function calculates the inverse of the special "matirx"
## object created with the 'makeCacheMatrix' function. However, if the inverse
## has already been calculated, the function grabs the inverse from the cache
## and skips the computation. Otherwise the function calculates the inverse of
## the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}