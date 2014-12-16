## Together, these functions will create a new matrix object,
## calculate the inverse, store it in the cache, and return it.

## Creates a matrix object that stores the original value as well as
## what will be the cached value (initially NULL).
## Also includes two functions: one to "set" (change) and one to "get"
## the inverse and the matrix, similar to OOP.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        getMatrix <- function() { x }
        
        setInv <- function(solve) {
                inverse <<- solve
        }
        
        getInv <- function() { inverse }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
                        setInv = setInv, getInv = getInv)

}

## Accesses the object created by makeCacheMatrix() and returns the
## value of the matrix used to create it.
## If inverse hasn't been calculated, it will calculate it and store
## it in the object created in makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getMatrix()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
