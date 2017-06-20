## Create one function which defines an environment to cache
## a matrix and its invserse, and a second function which uses
## the first function to retrieve calculate the inverse of new 
## matricies or retrieve the cached inverse if already created

## Create special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invrse) inv <<- invrse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Compute inverse of matrix returned by makeCacheMatrix; 
## if already calculated retrieve inverse from cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
            message("get cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
