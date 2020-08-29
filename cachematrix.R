## Cache and compute the inverse of a matrix  
## Note: Don't work if its not invertible and don't detect it

## Makes a Cache for the inverse, based on the supplied Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Solves the inverse for the cached Matrix supplied using makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
