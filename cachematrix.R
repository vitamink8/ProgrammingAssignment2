## Together, these functions will create a new matrix object,
## calculate the inverse, store it in the cache, and return it.

## Creates a matrix object that stores the original value as well as
## what will be the cached value (initially NULL).
## Also includes two functions: one to "set" (change) and one to "get"
## the inverse and the matrix, similar to OOP.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #initializes inverse to nothing
        setMatrix <- function(y) { # initializes matrix object
                x <<- y # caches matrix
                inverse <<- NULL # sets inverse to NULL
        }
        
        getMatrix <- function() { x } #returns the matrix
        
        setInv <- function(solve) { #calculates the inverse
                inverse <<- solve
        }
        
        getInv <- function() { inverse } #returns the inverse
        
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
        
        ## Accesses the cache, if there is something there
        if(!is.null(inverse)) {
                message("getting cached data")
                ## Returns cached data
                return(inverse)
        }
        
        data <- x$getMatrix() #Gets the matrix
        inverse <- solve(data, ...) # Calculates the inverse
        x$setInv(inverse) # "sets" the inverse
        inverse #Returns the inverse
}
