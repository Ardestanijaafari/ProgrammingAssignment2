# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it rep-
# eatedly. The following two functions cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve function returns the inverse of the matrix. At first, it ch-
# ecks if the inverse of the matrix has already been computed. If so, it
# reports the invese, write the message "getting cached data", and skips
# the computation. Otherwise, it calculates the inverse, store it in the
# cache using setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse ()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Example:
## > x <- matrix(1:4,2,2)
## > a <- makeCacheMatrix(x)
## > a$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## No cache at first, therefore cacheSolve function computes the inverse
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## In the second run, cacheSolve uses the cache and write the message
## > cacheSolve(a)
## getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5 
