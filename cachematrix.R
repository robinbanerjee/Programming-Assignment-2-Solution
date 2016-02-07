## These two functions allow for the "cacheing" of the inverse of a 
## matrix so that the computation does not have to be performed more 
## than once.

## makeCacheMatrix - creates a list of functions allowing for the 
## storage of the inverse of the orignial matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y){
    x <<- y
    s<<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) s <<- solve
  
  getinverse <- function() s
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve - checks if the data matrix associated with the argument 'x', 
## which is created with the makeCacheMatrix function, has already
## been inverted. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve will
## retrieve the inverse from the cache. Otherwise, it will invert x using 
## Solve, store the value in a cache variable, and return the inverse
## as a matrix.

cacheSolve <- function(x, ...) {
  
  ## Check if the data associated with the list 'x' has been inverted 
  ## before. If so, return the previously calculated inverse. 

  s <- x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  ## If the data has not been inverted before, load and invert 
  ## it using "solve", store the value, and return the inverse matrix.
  
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  x$getinverse()
  s
  
}
