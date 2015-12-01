##
## Inverting a matrix is a computationally expensive operation.  If we are
## working on the same matrix many times we can calculate the inverse once
## and save it so that we don't need to calculate it again the next time.
##
## This is a pair of functions to cache the expensive operation of inverting
## a matrix.
##
## To use these functions first create your matrix like this:
##
##    myMatrix <- makeCacheMatrix(matrixdata)
##
## To get the matrix data:
##
##    myMatrix$get
##
## To get the inverse
##
##    cacheSolve(myMatrix)
##
## To set the matrix to new contents:
##
##    myMatrix$set(matrixdata2)
##
## To get the inverse directly if cacheSolve has set it.
##
##    myMatrix$getinverse()
##
##
## This could be simpler to use if we replaced the getinverse function with the
## contents of the cacheSolve function.
##


## Return a list to be our matrix "object" with a number of functions. Using
## a closure and the <<- operator, it carries its data and potentially cached
## inverse around with it.

makeCacheMatrix <- function(x = matrix()) {
    
    # This is the parent environment of the functions, containing 'x' (from
    # the formal parameters) and 'inverse' which we set here.  In the
    # functions below the <<- operator will set them in this environment.
    
    inverse <- NULL
    
    get <- function() {
        x
    }
    
    set <- function(y) {
         x <<- y
         inverse <<- NULL
    }
    
    getinverse <- function() {
        inverse
    }
    
    setinverse <- function(y) {
        inverse <<- y
    }
    
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Calculate the inverse of a CacheMatrix returned by the makeCacheMatrix
## function. If we have previously cached the result, return the cached
## copy. Otherwise calculate the inverse and cache it before returning it.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    if (is.null(inv)) {
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
    }
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
