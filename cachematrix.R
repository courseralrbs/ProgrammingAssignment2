## Functions to create a object, that store a matrix and the inverse, and also 
## calculate the inverse

## Create a cache matrix object, that contains methods to set and get
## the matrix and the calculated inverse matrix

# @param x is the matrix which this object stores
makeCacheMatrix <- function(x = matrix()) {
    # x contains the matrix
    # i contaisn the inverse of the matrix
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInv <- function(iM) i <<- iM
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Calculates the inverse matrix for the given matrix cache object and return 
## it. It only calculates the inverse of the matrix, if it not calculated yet, 
## else it used the already saved inverse matrix.

# @param x is the matrix cache object
# @param ... additional options for solve function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # check if inverse is already calculated
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if inverse is not calculated yet, do it now
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}
