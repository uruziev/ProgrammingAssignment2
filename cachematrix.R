# Coursera Data Science 
# R Programming: Programming Assignment 2
# October 25, 2015
# Ulugbek Ruziev

# "makeCacheMatrix" function creates a vector to calculate and cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL   ## The inverse starts as NULL
  set <- function(y) {
  x <<- y   
  inv <<- NULL } ## Assign new argument as a stored value and reset inverse calculation
  get <- function() x  # Return internal object
    
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}

# "cacheSolve" function returns the inverse of square matrix and caches the result into CacheMatrix object.

cacheSolve <- function(x, ...) {
        # Returns an inversed matrix of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)  # Calculates the inverse
  x$setinverse(inv)
  inv
}