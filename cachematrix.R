## These functions work similar to the makeVector and cacheMean examples

## makeCacheMatrix takes a matrix as an argument and creates a new environment
## storing the value of the matrix (x) and its inverse (i).
## The function returns a list of functions to access those variables.

## cacheSolve takes a "cache matrix" object which was created with the makeCacheMatrix function
## and returns its inverse



## x: input matrix
## i: inverse of the matrix (default = NULL)
## Returns a list containing four functions:
## set: function to set the stored matrix
## get: function to get the stored matrix
## setInverse: function to set the inverse of the matrix
## getInverse: function to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                i <<- inverse
        }
        getInverse <- function() {
                i
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## x: input "cache matrix"
## i: inverse of the stored matrix
## data: stored matrix
## Returns the inverse of the stored matrix

cacheSolve <- function(x, ...) {        
        i <- x$getInverse()
        
        ## check if the inverse was already calculated
        if(!is.null(i)) {
                
                ## return the inverse
                message("getting cached data")
                return(i)
        }
        
        ## else: calculate and set and return the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
