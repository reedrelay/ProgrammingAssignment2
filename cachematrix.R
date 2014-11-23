## These functions allow the creation of a matrix-like object (defined to be 
## invertible) where the object will store (cache) the last generated inverse 
## and a inverse computing function which will retrieve the stored result when
## it exists.

## makeCacheMatrix() takes a matrix() as an argument (defaulting to an empty one)
## and returns an object containing the matrix which allows cacheSolve() to cache
## the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(m) {
        x <<- m
        minv <<- NULL
    }
    get <- function() x 
    getinv <- function() minv 
    setinv <- function(i) {minv <<- i}
    list(set =set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve works with the "matrix" objects returned by makeCacheMatrix() to
## store and retrieve precomputed inverses (and to calculate them if there are
## no precomputed ones.)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if (!is.null(xinv)) {
            print("Returning cached value.")
            return(xinv)
        }
        mtrx <- x$get()
        xinv <- solve(mtrx,...)
        x$setinv(xinv)
        xinv
}
