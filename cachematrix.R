## This function takes a invertible(non-singular,square)matrix as input,
## converts the matrix to a special matrix and checks for its cached 
## inverse. If cache value exists, it skips the calculations, else, it
## calculates and returns the inverse of the matrix.

## Function matrix.inv returns the inverse.
## Functions makeCacheMatrix and CacheSolve are support functions
## called to execute matrix.inv 

matrix.inv<-function(x=matrix()) cacheSolve(makeCacheMatrix(x))

## Function makeCacheMatrix converts input matrix to 
## a special "matrix", which is really a list containing 
## functions to :
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    
## Function to set X to cache    
    set <- function(y) {
        x <<- y
        I <<- NULL
    }

## Function to get input matrix
    get <- function() x

## Funtion to set the inverse of matrix to cache
    setinverse <- function(inverse) I <<- inverse

## Function to get the inverse of matrix from cache
    getinverse <- function() I
    
## List converting input matrix to special matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function cacheSolve checks for the cached Inverse of the 
## special "matrix" created with the above function. If so, it gets 
## the Inverse from the cache and skips the computation. Otherwise, it 
## calculates the Inverse of the matrix, sets the value of 
## the Inverse in the cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached inverse of matrix")
        return(I)
    }
   
## Call input matrix
    data <- x$get()
## Calculate the inverse of the matrix
    I <- solve(data, ...)
## Set the matrix inverse to cache    
    x$setinverse(I)
## Print inverse to screen    
    I
}
