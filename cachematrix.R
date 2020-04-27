## The aim of this assignment is to write a pair of functions
## that cache the inverse of a matrix

## makeCacheMatrix is a function that creates a special "matrix" that can
## cache the inverse for the input

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
        x <<- y
        inv <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) inv <<- inverse
   getInverse <- function() inv
   list(set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## The function here computes the inverse of the matrix created by makeCacheMatrix as seen above
## If the inverse was already calculated and the matrix did not change then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inv <-x$getInverse()
   if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
   }
   mat <- x$get()
   inv <- solve(mat,...)
   x$setInverse(inv)
   inv
}
