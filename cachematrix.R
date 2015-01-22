## Programming Assignment 2
## Guillermo Soriano

## Function makeCacheMatrix creates a list 
## containing functions to set matrix,
## get matrix, set inverse, get inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
        x <<- y
        i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i<<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## It is a function that calculates the inverse if
## its content is NULL, otherwise it will call it
## from cache

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
