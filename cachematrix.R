## Two functions that are used to create a special object
## that stores a matrix and caches its mean.

## makeCacheMatrix creates a special "matrix", which is 
## is really a list containing a function to set the value
## of the matrix, get the value of the matrix, set the value
## of the inverse and get the value of the inverse

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

## calculate the inverse of the special matrix above by 
## checking if the inverse was already calculated and 
## returning that or calculating and caching it

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
