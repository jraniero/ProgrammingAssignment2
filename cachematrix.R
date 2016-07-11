## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates list object that contains a matrix, its inverse
## and get/set functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #On matrix creation, set inverse to null
  set <- function(y) {
    x <<- y 
    inv <<- NULL #On matrix value change, set inverse to null
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve gets an 'makeCacheMatrix' object type and returns the
## inverse of the matrix inside that object. However, the function
## will store the inverse in the 'makeCacheMatrix' cache, so that
## the inverse does not need to be recalculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("solving inverse")
  data <- x$get() #Get matrix
  inv <- solve(data, ...) #Calculate inverse matrix
  x$setInverse(inv) #Store inverse matrix in cache
  inv
}
