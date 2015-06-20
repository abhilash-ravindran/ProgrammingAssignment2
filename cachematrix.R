## Matrix inversion computations are usually costly and there are benefits to caching the inverse. The two functions 
## makeCacheMatrix and cacheSolve used together provides caching of matrix inverse value.

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## The function returns list of functions to do the following
##    set the value of the matrix. This is also used to invalidate the cache when a new matrix is set.
##    get the value of the matrix.
##    setinverse the value of the inverse.
##    getinverse the value of the inverse. This is the cached inverse matrix if cached.
## Usage makeCacheMatrix(matrix)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve
## This function uses the list returned by makeCacheMatrix and returns the inverse of the matrix.
## The inverse matrix is retrived from the cache if it exists, otherwise the inverse is calculated and stored in cache.
## Usage cacheSolve(matrixObject)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
