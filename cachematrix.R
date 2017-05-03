## There are benefits to caching computational heavy operations e.g. matrix inversion. 
## The assigment was to write two functions which create a special object that stores a matrix and caches 
## the inverse of the matrix.

## This function "makeCacheMatrix" creates a special object that can cache the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function "cacheSolve" solves / calculates the inverse of the matrix from the function "makeCacheMatrix".
## If the the inverse of the matrix alread has been solved and cached then inverse will be retrieved from the cache (and thereby saving time)

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}


