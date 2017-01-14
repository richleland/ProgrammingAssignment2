## These functions can be used to create a special "matrix" object that acts as
## a cache for the inverse of a matrix, which can be a costly computation.
##
## Sample code follows...
## 
## Create a special matrix:
##     > special_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##
## Solve the inverse of the matrix:
##     > cacheSolve(special_matrix)
##     computing inverse of matrix
##
## The original matrix value:
##     > special_matrix$get()
##          [,1] [,2]
##     [1,]    1    3
##     [2,]    2    4
##
## The computed, cached inverse:
##     > special_matrix$getinverse()
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##
## Note how running cacheSolve again retrives the inverse from cache:
##     > cacheSolve(special_matrix)
##     getting cached data
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5
##
## Set a new matrix value, which wipes out the cache:
##     > special_matrix$set(matrix(2:5, 2, 2))
##     > special_matrix$getinverse()
##     NULL
##
## Running cacheSolve again will force re-computing, as the cache is empty:
##     > cacheSolve(special_matrix)
##     computing inverse of matrix
##     > special_matrix$getinverse()
##          [,1] [,2]
##     [1,] -2.5    2
##     [2,]  1.5   -1


## The makeCacheMatrix function creates a special "matrix" object, which is
## really a list with method for getting/setting the matrix and getting/setting
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        cached_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        cached_inverse <<- inverse
    }
    getinverse <- function() cached_inverse
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## The cacheSolve function does the following:
##   - takes a special "matrix" object returned by the makeCacheMatrix function
##   - attempts to grab a cached version of the inverted matrix
##   - computes the inverse of the matrix if not already in cache

cacheSolve <- function(x, ...) {
    # grab solved matrix from cache and return if not null
    solved <- x$getinverse()
    if(!is.null(solved)) {
        message("getting cached data")
        return(solved)
    }
    
    # compute inverse of matrix, set it in cache
    message("computing inverse of matrix")
    uninverted <- x$get()
    inverse <- solve(uninverted, ...)
    x$setinverse(inverse)
    # return this as invisible so the special matrix object doesn't get output
    # every time we run this method
    invisible(x)
}
