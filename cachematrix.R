## Coursera: R Programming - Programming Assignment 2: Lexical Scoping
## These functions will cache the inverse of a matrix
## WRH 2014-07-22

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(mean) m <<- mean
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##            If the inverse has already been calculated (and the matrix has not changed), 
##            then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Test case 1:
## > mtrx<-matrix(c(2,4,2,5,2,7,6,8,5),3,3)
## > yy<-makeCacheMatrix(mtrx)
## > cacheSolve(yy) 
# created original inverse
## > cacheSolve(yy)
# gets cached inverse

## Test case 2:
## > mtrx<-matrix(c(3,6,5,9),2,2)
## > yy<-makeCacheMatrix(mtrx)
## > cacheSolve(yy) 
# created original inverse
## > cacheSolve(yy)
# gets cached inverse