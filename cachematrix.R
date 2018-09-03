## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## special "matrix" object 
## to cache the value of the inverse matrix so that when we need it again,
## it can be looked up in the cache rather than recomputed
##
## eg. 
## A<-matrix(1:4,n,n) # square matrix
## > A
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## cA <- makeCacheMatrix(A)
## summary(cA)
##            Length Class  Mode    
## set        1      -none- function
## get        1      -none- function
## setinverse 1      -none- function
## getinverse 1      -none- function
##

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## Inverse of a square Matrix 
## 
## first create a special "matrix" using makeCacheMatrix() function
## then cacheSolve(Matrix)
##
## eg. 
## A<-matrix(1:4,n,n) # square matrix
## > A
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## cA <- makeCacheMatrix(A) # special "matrix" object
## invA <- cacheSolve(cA)
## invA
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## invA2 <- cacheSolve(cA)
## getting cached data
##
## identical(invA,invA2)
## [1] TRUE

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) 
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
}
