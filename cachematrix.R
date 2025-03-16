

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	    inv <- NULL  # Variable to store the cached inverse
    set <- function(y) {
	            x <<- y
            inv <<- NULL  # Reset cached inverse when matrix is updated
	        }
        get <- function() x  # Return the original matrix
        setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
	    getInverse <- function() inv  # Retrieve the cached inverse
	    
	    list(set = set, get = get,
		          setInverse = setInverse,
			           getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been computed and cached, it retrieves it instead of recomputing.

cacheSolve <- function(x, ...) {
	    inv <- x$getInverse()  # Check if inverse is already cached
    if (!is.null(inv)) {
	            message("getting cached data")
            return(inv)
	        }
        data <- x$get()  # Get the matrix
        inv <- solve(data, ...)  # Compute the inverse
	    x$setInverse(inv)  # Cache the inverse
	    inv  # Return the inverse matrix
}

