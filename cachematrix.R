## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function will set the matrix, get the matrix, set the inverse of the matrix, and 
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(matrix) { #Set the matrix
      m <<- matrix
      i <<- NULL
  }
  get <- function() m #Get the matrix
  setInverse <- function(inverse) i <<- inverse #Set inverse
  getInverse <- function() i #Get inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

# Use solve to compute the inverse and use matrix multiplication

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #Get matrix inverse of x
  if(!is.null(m)) { #Returns the inverse if already there
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data #Solves the inverse using matrix multiplication
  x$setInverse(m)
  m     
}
