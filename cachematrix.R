## The following functions can create a special object that stores a matrix
## and cache's its inverse. With these functions, you can cache time-consuming
## computations of matrix inversion instead of compute them repeatedly (e.g. in
## a loop).

## makeCacheMatrix is a function that creates a special "matrix" object.
## This function stores a list of four functions:
## 1. get returns the matrix x stored in makeCacheMatrix
## 2. set changes the matrix x stored in makeCacheMatrix and restored to null
## the value of the variable inverse
## 3. setinverse stores the value of an input in a variable called inverse
## 4. getinverse returns the value stored in setinverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then this function retrieves
## the inverse from the cache. Otherwise, it computes and sets the
## matrix inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}