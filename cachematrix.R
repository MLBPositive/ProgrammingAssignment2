## This source file contains functions manage a cached inverse of square matrix.
## Based on solve() function, and its ability to calculate the inverse of a matrix.
#

## makeCacheMatrix - function to store and retrieve (get/set) a square matrix,
## and the inverse of the matrix.
##      set - cache the value of the vector
##      get - retrieve the value of the vector
##      setinverse - cache the value of the vector
##      getinverse - retrieve the value of the vector
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve - function that works with cached matrix, and computes inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
