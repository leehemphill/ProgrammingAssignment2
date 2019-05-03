## A pair of functions that cache the inverse of a matrix

## create a function that stores the inverse of a matrix
## and a function to create that matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Return a matrix that is the inverse of 'x'.  Either calculate the inverse,
##  or if it's already been calculated, use that stored inverse

cacheSolve <- function(x, ...) {
## 
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
