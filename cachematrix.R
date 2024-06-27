## MakeCacheMatrix is a function that creates a special matrix object
## that can cache its inverse
## CacheSolve computes the inverse of the special matrix if available or
## computes it otherwise

## MakeCacheMatrix function consists of a list of other functions to set and
## get the value of the matrix, set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The CacheSolve function either gets the cached value of the matrix from the
## function above or calculates it from the matrix created above
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()  ## Return a matrix that is the inverse of 'x'
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
