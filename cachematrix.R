## this R file contains two functions to illustrate the use of a cached object
## to avoid recomuptation of the inverse of a matrix if the matrix did not change
## the two functions are: makeCacheMatrix and cacheSolve



## makeCacheMatrix creates a special matrix object that can cache its inverse
## this function returns a list of functions as follows:
##    setMatrix() to set the values inside the matrix
##    getMatrix() to get the values stored inside the matrix
##    getInverse() to get the values of the inverse of the matrix
##    cacheInverse() to get the cached values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # initially, set cache to NULL since no inverse was computed first
    cache <- NULL
    
    # store new values in matrix
    setMatrix <- function(newMatrixValues) {
      x <<- newMatrixValues
      
      # set cache to NULL since new values are stored in matrix
      cache <<- NULL
    }
    
    # returns the stored matrix
    getMatrix <- function() {
      x
    }
    
    # returns the cached matrix
    getInverse <- function() {
      cache
    }
    
    
    # cache the given argument 
    cacheInverse <- function(solve) {
      cache <<- solve
    }
    
    # return a list. Each named element of the list is a function
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix, 
         cacheInverse = cacheInverse, 
         getInverse = getInverse)
  }


## cacheSolve computes the inverse of the special matrix object returned by makeCacheMatrix. 
## if the inverse matrix has been computed previously and the matrix has not changed, then
## this function retrieves the inverse from the cache
## we assume that the matrix object is invertible

cacheSolve <- function(x, ...) {

    # get the inverse from cache in case it was computed before
    inverse <- x$getInverse()
    
    # if previously cached return it from cache
    if(!is.null(inverse)) {
      message("getting cached data")
      
      return(inverse)
    }
    
    # if did not return the cached value (i.e., null inverse, not computed before)
    # Get the matrix, and compute its inverse and store it in cache
    data <- x$getMatrix()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    
    # return the inverse
    inverse
  }
  
## test it with 3x3 invertible matrix (from http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices11-2009-1.pdf)

