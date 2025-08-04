## makeCacheMatrix function

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## - set the matrix
## - get the matrix
## - set the inverse of the matrix
## - get the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Cached inverse # nolint
    
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Invalidate cache when new matrix is set
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve function

## computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute inverse
    x$setinverse(inv)      # Cache inverse
    
    inv
}


## I have tested the above functions with the following example:

# my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Create special matrix object
# cached_matrix <- makeCacheMatrix(my_matrix) 

# First call: calculates and caches the inverse
# cacheSolve(cached_matrix)

# Second call: retrieves inverse from cache
# cacheSolve(cached_matrix) 