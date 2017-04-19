## given a invertible the following two function will calculate the inverse matrix or get the inverse matrix from cache


## creates an matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
     set <- function(y) {
        x <<- y
         mx <<- NULL
        
    }
     get <- function() x
     setinverse <- function(computed) mx <<- computed
     getinverse <- function() mx
     list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## computes the inverse of the matrix returned by the makeCacheMatrix() above. If the inverse has been calculated the cacheSolve() will retieve the inverse from cache. 
## If not calculated data gets the matrix stored with makeCacheMatrix() 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mx <- x$getinverse()
     if (!is.null(mx)) {
        message("getting cached data")
         return(mx)
        
    }
     data <- x$get()
     mx <- solve(data, ...)
     x$setinverse(mx)
     mx
}
