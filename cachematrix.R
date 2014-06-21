## Put comments here that give an overall description of what your
## functions do

## 
## makeCacheMatrix creates the special matrix that will be used to
## cache (store and retrieve) the inverse of a sqaure matrix
##

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(invx) inv <<- invx
    
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##
## cacheSolve computes the inverse of the special matrix that was
## previously created in the makeCacheMatrix function above
##

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()

    # if there is a cache hit, return the data from the cache
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # if there is a cache miss, get the matrix x and compute the inverse
    
    data <- x$get()
    inv <- solve(data, ...)
    
    # store the newly computed inverse of the matrix in the cache and return
    # the computed inverse of the matrix
    
    x$setinv(inv)
    inv
    
}
