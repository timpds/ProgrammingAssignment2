## The following functions allows a special "matrix" object to be created
## which can store a matrix, and cache its inverse after it has been
## calculated.  This provides an efficient way of retrieving the
## inverse of a matrix without having to compute it each time it is
## required.
## 
## Author:  TC
## Date:    22/12/2015


## This function creates a special "matrix" object to store the cache
## of the inverse of a given matrix.
## 
## Input:   a single matrix object.
## Output:  a list of functions to get and set (cache) the matrix object
##          and get and set (cache) its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y      # Cache the input matrix.
        i <<- NULL   # Cache the inverse as NULL.
    }
    get <- function() x
    setinv <- function(solve) i <<- solve # Cache the input inverse.
    getinv <- function() i
    
    # Return a list object with the utility functions above.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function computes the inverse of any special "matrix" object
## created by running makeCachedMatrix()
## 
## Input:   a single special "matrix" object created by running makeCachedMatrix().
## Output:  the inverse matrix of the original matrix object stored by
##          makeCachedMatrx()

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("***Returning cached inverse***")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}


## Some testing functions below:

## Create a matrix, then create a special "matrix" object from it,
## then finally solve it once, then twice, observing the cached
## version output the second time.

matrixCacheTest1 <- function() {
    
    baseMatrix <- matrix(1:4, nrow = 2, ncol = 2)
    message("The original matrix is:")
    print(baseMatrix)
    
    baseMatrixCached <- makeCacheMatrix(baseMatrix)
    message("Check that the inverse starts out as NULL:")
    print(baseMatrixCached$getinv())
    
    message("Calculate the inverse and store in the cache:")
    print(cacheSolve(baseMatrixCached))
    
    message("Now solve the inverse again, noting the returned cache:")
    cacheSolve(baseMatrixCached)
    
}