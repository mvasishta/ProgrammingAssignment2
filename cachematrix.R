## Program created for assignment submission in Coursera
## Author - mvasishta
## Date - 03/13/2016

## Sample Call
## source("cachematrix.R")
## x <- matrix(1:4,2,2)
## y <- makeCacheMatrix()
## y$set(x)
## cacheSolve(y)

## makeCacheMatrix creates a special matrix object and caches its inverse.  
## It provides functions to interact with environment variables
makeCacheMatrix <- function(x = matrix()) {
    MyMatrix <- NULL
    MyInverseMatrix <- NULL
      set <- function(x) {
          message("Creating a matrix object")
          MyMatrix <<- x  
          message("Caching the inverse")
          MyInverseMatrix <<- solve(MyMatrix)
        }
        get <- function() {
                MyMatrix
        }
        getInverse <- function() {
                MyInverseMatrix
        }
  list(set=set,get=get,getInverse=getInverse)
}

## cacheSolve function fetches inverse matrix from cache if found.  Otherwise, it computes the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  OutputMatrix <- NULL
  ## Check if the argument x is of type makeCacheMatrix
    if(length(x) ==3 && names(x[3]) == "getInverse")
    {
  ## Check if x is initialized and has cached inverse matrix
      if(!is.null(x$get())) {
          if(is.null(x$getInverse())) {
            message("Inverse matrix not cached. Computing..")
            OutputMatrix <- solve(x$get())
          }
          else {
            message("Cached inverse matrix found. Fetching..")
            OutputMatrix <- x$getInverse()
          }
      }
      else
      {
        message("Argument has not initialized a valid matrix. Please run set function")
      }
    }
    else
      message("Argument passed is not a makeCacheMatrix Object")
  OutputMatrix
}
