## Matrix inversion is a costly computation.
## These functions take a matrix as input, 
## retrieve its inverse from a cache if already computed before,
## and if not yet available, calculates the inversion 
## and stores is in the cache for reuse.
## R Programming assignment submitted by Stefaan Verbeure, Nov 18th 2016  


## makeCacheMatrix creates an object 'im' 
## to store an inverted matrix in a cache

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        getim <- function() im
        setim <- function() im <<- solve(x)
        list(set = set, get = get, setim = setim, 
             getim = getim)
}

## cacheSolve computes the inverse 'im' of the matrix returned by makeCacheMatrix.
## If the inverse for that matrix was already available in the cache, 
## then cacheSolve retrieves it from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        im <- x$setim()
        print(im)
}
