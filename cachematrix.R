## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix
## input: an invertible matrix x
## output: a list of functions that can be applied
## to the original matrix x:
## get - returns the original matrix
## set - sets the value of the matrix
## setinv - computes and sets the inverse of the matrix
## getinv - gets the cached value of the matrix inversion
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## cacheSolve computes the inverse of a matrix x
## it first tries to get a cached value of the inverse
## if null it computes inverse and caches it.
## input: x should be a makeCacheMatrix object
## output: inverse of matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
