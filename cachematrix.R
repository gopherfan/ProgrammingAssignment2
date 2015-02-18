## Put comments here that give an overall description of what your
## functions do
## 
## This file contains two functions:
## makeCacheMatrix(x)
## cacheSolve(x, ...)
## These two functions can be used together to construct a simple datastructure
## for caching the inverse of a matrix, thus eliminating the need to recompute
## the inverse for the same matrix
##
## By: Fan Jia
## 02/17/15


## Write a short comment describing this function
## This function construct a list of four functions from a matrix 
## for caching and obtaining a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    invIsCurrent <- FALSE
    set <- function(y){
        if(y!=x){
            x <<- y
            mInv <<- NULL
            invIsCurrent <<- FALSE
        }
    }
    get <- function() x
    setInv <- function(inv){
        mInv <<- inv
        invIsCurrent <<- TRUE   
    }
    getInv <- function() mInv
    isCurrent <- function() invIsCurrent
    list(set = set, get = get, 
         setInv = setInv, 
         getInv = getInv, 
         isCurrent = isCurrent)
}


## Write a short comment describing this function
## This function takes the list constructed by makeCacheMatrix as input
## and returns the inverse of the corresponding matrix. If the inverse 
## of the matrix is in the cache and the matrix has not changed, the function
## will simply return the cached inverse, otherwise solve() is called to 
## compute the inverse of the matrix
cacheSolve <- function(x, ...) {
    m <- x$get()
    inv <- x$getInv()
    if(!is.null(inv) && x$isCurrent){
        message("getting cached inverse")
        return(inv)
    }
    else{
        inv <- solve(m)
        x$setInv(inv)
        return(inv)
    }
}
