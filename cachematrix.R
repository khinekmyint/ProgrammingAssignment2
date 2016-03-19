# makeCacheMatrix
# Creates a special "matrix" object that can cache its inverse.
# The object does not calculate the inverse, just saves it inside.
# Saves the matrix to variable x and its inverse to variable m in scope.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set: set the matrix and make the inverse of matrix to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get: return the matrix
  get <- function() x
  
  # setinverse: sets cached inverse of matrix
  setinverse <- function(slove) m <<- solve
  
  # getinverse: returns inverse of matrix
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#cacheSolve
# Function to get the inversed matrix from a special object
# Takes the object of that type as an argument 'x', checks if the inverse value is already
# cached, and if it is returns the cached value; if not, this function calculates the
# inverse for the matrix saved in the 'm', saves it into 'm' cache using method 'setinverse'
# and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
