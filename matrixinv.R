## This function takes a invertible(non-singular,square)matrix as input,
## converts the matrix to a special matrix and checks for its cached 
## inverse. If cache value exists, it skips the calculations, else, it
## calculates and returns the inverse of the matrix.

## Function matrix.inv returns the inverse.
## Functions makeCacheMatrix and CacheSolve are support functions
## called to execute matrix.inv 

matrix.inv<-function(x=matrix()) {

## Check if matrix is square
    if(nrow(x)!=ncol(x)) stop("Row & columns not equal, Pls input
                              a square matrix")
## Check if matrix is non-singular
    if(det(x)==0) stop("Matrix is singular,input non-singular matrix")
    
    
## Convert input matrix to a special "matrix", which is really 
## a list containing functions to :
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    inv.lst=list(set = set, get = get,
         setinverse = setinverse,
    getinverse = getinverse)

## Check for the cached Inverse of the special "matrix" 
## created with the above function. If so, it gets 
## the Inverse from the cache and skips the computation. Otherwise, it 
## calculates the Inverse of the matrix, sets the value of 
## the Inverse in the cache and returns the inverse

    ## Return a matrix that is the inverse of 'x'
    Inv <- inv.lst$getinverse()
    if(!is.null(Inv)) {
        message("getting cached inverse of matrix")
        return(Inv)
    }
    data <- inv.lst$get()
    Inv<- solve(data)
    inv.lst$setinverse(Inv)
    Inv
}
