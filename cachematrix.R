## The functions below are used to create a special object that stores a matrix and caches
## its inverse

## makeCacheMatrix creates a special "matrix", which is a list containing functions to

## 1. set the value of the matrix
## 2. get the value of the vector
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse og the matrix

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


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix
## It first checks to see if the inverse has already been calculated. If that is
## the case, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function

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
