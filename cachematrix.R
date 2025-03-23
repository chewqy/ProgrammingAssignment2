## The makeCacheMatrix creates a special "matrix" object.
## This object stores the matrix and provides function to access and modify it.
## The cacheSolve function calculates the inverse of the matrix.
## If the inverse is already cached, it retrieves it. If not, it calculates and stores it.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvers <- function(invers) m <<- invers
        getinvers <- function() m
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m
}
