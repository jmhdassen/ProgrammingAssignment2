## Put comments here that give an overall description of what your
## functions do

## This pair of functions create and manipulate an invertible matrix as a closure. 
## Once the matrix has been inverted the  result is cached in the closure. 

## A short comment describing makeCacheMatrix
## makeCacheMatrix creates the closure of a Matrix x.
## the closure contains 4 operations: set,get, setSolve, getSolve.
## The closure contains 2 variables, x and Minv, where x is the Matrix and Minv its Inverse.
## set(x1) will update the matrix and null its inverse.
## get() will retrieve the matrix x.
## setSolve(inv) will set the inverse. It should be used only by the function CacheSolve.
## getSolve() will retrieve the Matrix inverse (if it has been calculated)


makeCacheMatrix <- function(x = matrix()) {
    Minv<- NULL
    set <- function(y){
        x<<- y
        Minv<<- NULL
    }
    get<-function() x
    setSolve<-function(solve) Minv <<- solve
    getSolve<-function() Minv
    list(set=set, get=get,setSolve=setSolve,getSolve=getSolve)
    
}


## A short comment describing cacheSolve:
## cacheSolve checks whether the matrix inverse has already been cached in Minv.
## if yes, then it will return Minv, together with a suitable message.
## if no, then it will calculate the inverse of x and store it in the Minv container.
## cacheSolve uses accessors defined in makecacheMatrix.
##
## Usage:
## A1<-makeCacheMatrix(some square matrix object)    creates the closure A1
## cacheSolve(A1)      returns the inverse for A1
## A1$get() returns the matrix
## A1$getSolve() returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Minv<- x$getSolve()
    if(!is.null(Minv)){
        message("getting cached data")
        return(Minv)
    }
    data<- x$get()
    Minv<-solve(data,...)
    x$setSolve(Minv)
    Minv
}
