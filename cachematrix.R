## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Allows the input matrix 'x' to be stored in cache
##                  Allows cache of inverse of 'x' 
##                  Allows retrieval of cached values

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve:  Allows computation of matrix inverse
##              If inverse cached, retrieves cached value
##              instead of re-calculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                message("Retrieving cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
