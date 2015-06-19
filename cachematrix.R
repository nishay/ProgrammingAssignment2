# Functions in this R script creates a square invertible matrix, computes
# the inverse of the matrix and store the inverse in a cached environment
# for later use.
# Working with cached results improves the performance of otherwise
# time-consuming computations.
# 
# makeCacheMatrix creates a special matrix 'inv', which contains the functions to 
# a) set the matrix, b) get the matrix, and is used by cacheSolve function to
# c) create and store the inverse of the matrix in cache,
# d) get the inverse of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        # a) create the matrix and initialise to NULL
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # b) get the matrix
        get <- function() x
        
        # c) create and store the inverse of the matrix in cache
        setInv <- function(inverse) inv <<- inverse
        
        # d) get the inverse of the matrix from cache
        getInv <- function() inv
        
        ## Return a list of functions
        list(set=set, get=get, setInv=setInv, getInv=getInv)
}


# cacheSolve function gets the inverse of the square invertible matrix, created by
# makeCacheMatrix function, from cache. If it does not exist in cache, it creates
# the inverse of the matrix and stores in cache environment.

cacheSolve <- function(x, ...) {
        # get inverse from cache
        inv <- x$getInv ()
        
        # if the inverse exists cache, return the inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # get the square invertible matrix, use solve() to inverse it
        sqMatrix <- x$get()
        inv <- solve(sqMatrix, ...)
        # store inv in cache and return the inverse
        x$setInv(inv)
        return(inv)
}