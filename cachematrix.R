## The below functions will allow for storing(caching) the inverse of a matrix, 
## so if the content of the matrix doesn't change R can quicly look up and use cached value.


## makeCacheMatrix is a function that stores a list of functions that will allow for caching 
## the inverse of special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  set <- function(y) { 
    x <<- y 
    inverse <<- NULL 
  } 
  get <- function() x 
  setsolve <- function(solve) inverse <<- solve 
  getsolve <- function() inverse 
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve) 
} 


## The following function checks to see if the inverse has already been calculated: 
## if yes, it will use it, 
##if not will calculate it from scratch based on the special "matrix" object from the above function

cacheSolve <- function(x, ...) {
  inverse <- x$getsolve() 
  if(!is.null(inverse)) { 
    message("getting cached data") 
    return(inverse) 
  } 
  data <- x$get() 
  inverse <- solve(data, ...) 
  x$setsolve(inverse) 
  inverse 
} 