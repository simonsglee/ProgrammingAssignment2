## These two functions cache the inverse of a matrix
## and save costly computation involved with the matrix inversion process

## Creates a matrix object that can cahe its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set =set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes the inverse of the matrix returned by makeCacheMatrix
## function. Retreives the inverse from cache if it already has been
## calculated

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
