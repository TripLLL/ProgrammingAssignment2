#************************************
# Coursera - R Programming
# Week 3 - Programming Assignment
# 09.12.2016
#************************************


## the makeCacheMatrix is a function which creates a "matrix" object that 
## caches its inverse. It is a list containing functions to: 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      # 1. set the value of the matrix
            set <- function(y) {
                  x <<- y
                  inv <<- NULL
            }
      # 2. get the value of the matrix
            get <- function() x
      # 3. set the value of the inverse
            setinv <- function(inverse) inv <<- inverse
      # 4. get the value of the inverse
            getinv <- function() inv
      # create list with named objects (for functions)
            list (set = set,
                  get = get,
                  setinv = setinv, 
                  getinv = getinv)
}


## the cacheSolve function calculates the inverse of the "matrix" object created 
## before. If inverse has already been calculated, it gets the inverse from the 
## the cache, otherwise calculates the inverse and stores it in the cache for
## further use as long as the x value doesn't change.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
            inv <- x$getinv()
      ## if inverse is in cache, return stored inverse
            if (!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
      }
      ## if inverse is not in cache:
      # get data
      data <- x$get()
      # calculate inverse
      inv <- solve(data, ...)
      # put inversein cache for further use
      x$setinv(inv)
      # display inverse
      inv
}
