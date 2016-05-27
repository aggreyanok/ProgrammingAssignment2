## The makeCacheMatrix function creates a vector which is a list containing
## functions that; 
# 1. Sets the value of the invertible matrix i.e its elements and dimension (set function)
# 2. Gets the value of the invertible matrix (get function)
# 3. Sets the inverse of the invertible matrix (setInverse function)
# 4. Gets the inverse of the invertible matrix (getInverse function)

## Date modified: 27-05-2015

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL 
  }
  get <- function() x
  setInverse <- function(solve) I <<- solve
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


#The cacheSolve function computes the inverse of matrix created with
#the makeCacheMatrix function. It first checks to see if the inverse of the matrix
#has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it computes the inverse of the supplied matrix and sets its value in the cache via 
#the setInverse function.

cacheSolve <- function(x, ...) {
  I <- x$getInverse() 
  if(!is.null(I)) {
    message("Returning the cached inverse of the matrix")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
        
}
