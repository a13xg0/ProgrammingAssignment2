## This package covers a functinality of creating
## a cached matrix object and perform matrix inversion.
## Cahced Matrix inversion result will be stored in the 
## package cache to prevent a redundant computation.

## Function generates an object of cached matrix
## with additional functions to manage internal variables

makeCacheMatrix <- function(x = matrix()) {
	# inverse of the matrix
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


## Caluclates inversion of the cahced matrix object and store
## result in the cahce. At next requests function will return
## value from the object's internal cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
