## Functions can be used to cache a calculated inverse of a matrix and retrieve
## that cached inverse instead of recalculating it


## Write a short comment describing this function
## makeCacheMatrix creates an object that includes a matrix, its inverse 
## (if it has been calculated and cached already, NULL if not), and a list of
## functions to set or get the values of the matrix, and to set or get the inverse

makeCacheMatrix <- function(x = matrix()) {
      invertm <- NULL
      set <- function(y) {
            x <<- y
            invertm <<- NULL
      }
      
      get <- function() x
      
      setinvertm <- function(inverted) invertm <<- inverted
      getinvertm <- function() invertm
      
      list(set = set, get = get, setinvertm = setinvertm, getinvertm = getinvertm)
}


## Write a short comment describing this function
## Call cacheSolve on a matrix object of type makeCacheMatrix
## cacheSolve checks if the inverse of the matrix has already been cached
## it returns the inverse from cache if so, otherwise it calculates the inverse
## with solve() and caches and returns the inverse

cacheSolve <- function(x, ...) {
      invertm <- x$getinvertm()
      
      if(!is.null(invertm)){
            print("Getting cached data:")
            return(invertm)
      }
      
      z <- x$get()
      invertm <- solve(z, ...)
      x$setinvertm(invertm)
      invertm
}
