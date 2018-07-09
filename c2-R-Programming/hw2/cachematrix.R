## Put comments here that give an overall description of what your
## functions do

## Make a vector of a cached matrix and some functions to set, get, setinverse,
## getinverse

makeCacheMatrix <- function(A = matrix()) {
  inverse <- NULL
  set <- function(B) {
    A <<- B
    inverse <<- NULL
  }
  # Function to just 
  get <- function() A
  setinverse <- function(solve) inverse <<- solve 
  getinverse <- function() inverse
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}



## Write a short comment describing this function

cacheSolve <- function(A, ...) {
  ## Return a matrix that is the inverse of 'A'
  inverse <- A$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <-A$get()
  inverse <- solve(data, ...)
  A$setinverse(inverse)
  inverse
}
