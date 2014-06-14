## This function first checks for a cached inverse of a invertible matrix
## and fetches it, if not , it calculates the inverse.

## The matrix.inv function is used to calculate the inverse.
## Functions makeCacheMatrix and CacheSolve are support functions
## called to execute the matrix.inv function 

matrix.inv<-function(x=matrix()) cacheSolve(makeCacheMatrix(x))

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function checks for the cached Inverse of the 
## special "matrix" created with the above function. If so, it gets 
## the Inverse from the cache and skips the computation. Otherwise, it 
## calculates the Inverse of the matrix and sets the value of 
## the Inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached inverse of matrix")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}
