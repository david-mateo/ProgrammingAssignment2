## Efficient matrix inversion with cache.

## Create a matrix-like list
## Create a special object that contains a matrix and a cached version of
## its inverse. The result is a list containing functions to
##      set:      set the value of the matrix
##      get:      get the value of the matrix
##      setsolve: set the value of the inverse
##      getsolve: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) s <<- inverse
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Calculate the inverse of a matrix contained in a list
## created with *makeCacheMatrix*. This function will
## check for a pre-computed cache version of the inverse
## and return that. If none is found, the inverse is
## computed with the *solve* function.
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
        }else{
                data <- x$get()
                s <- solve(data, ...)
                x$setsolve(s)
        }
        s
}


