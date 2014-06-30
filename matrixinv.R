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
    

## Check for the cached Inverse of the special "matrix" 
## created with the above function. If so, it gets 
## the Inverse from the cache and skips the computation. Otherwise, it 
## calculates the Inverse of the matrix, sets the value of 
## the Inverse in the cache and returns the inverse

    ## Return a matrix that is the inverse of 'x'
    Inv <- if(!exists("minv")) NULL
           else if (!identical(x,minv$get())) NULL 
           else minv$getinverse()
    
    if(!is.null(Inv)) {
        message("getting cached inverse of matrix")
        return(Inv)
    }
    minv<<-makeCacheMatrix(x)
    data <- minv$get()
    Inv<- solve(data)
    minv$setinverse(Inv)
    Inv
}

## makeCacheMatrix converts input matrix to special matrix 
## by defining Following functions :
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse


makeCacheMatrix <- function(x=matrix()) {
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


