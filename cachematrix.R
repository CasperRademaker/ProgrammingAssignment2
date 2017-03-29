## These functions create a special matrix and a function to calculate the inverse of the matrix,
## but will check first whether the cached result is not already available. If already available, 
## it will just grab the cached result. 

## This function creates a special matrix, which is really just a list containing functions to:

## set the values of the matrix
## get the values of the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inputinverse) inverse <<- inputinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will check whether the inverse of the input matrix is already cached or not, if it is
## already cached then it will just grab the cached data. Otherwise it will calculate the inverse and
## cache the result

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
}
