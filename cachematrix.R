## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute.  
## If the contents of a matrix are not changing, 
## it may make sense to cache the value of the inverse so that when we need it again, 
## it can be looked up in the cache rather than recomputed.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setMatInverse  <- function(inverse) i  <<- inverse
        getMatInverse  <- function() i
        list(set= set, get = get, 
             setMatInverse = setMatInverse,
             getMatInverse = getMatInverse)
}


## Computes the inverse of the given matrix. If the matrix and the inverse exits
## in the cache, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 i  <- x$getMatInverse()
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setMatInverse(i)
        i
}
