## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set a new matrix and reset cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the inverse
    getInverse <- function() inv
    
    # Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # If inverse is already cached
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # Compute inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    inv
}
