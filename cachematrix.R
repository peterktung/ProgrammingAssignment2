## Creates a matrix object that caches inverse calculation, and retrieves
## inverse value if cached.  Otherwise, calcuate and cache. 

## function that creates a matrix object that caches inverse calculation
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves cached value if available, otherwise calculate inverse. Assumes
## matrix has an inverse.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        m
    }
    mat <- x$get()
    inverse = solve(mat)
    x$setinverse(inverse)
    inverse
}

