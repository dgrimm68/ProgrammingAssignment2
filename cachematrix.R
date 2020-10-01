##Caching the Inverse of a Matrix
##Matrix inversion is usually very computationally costly and there may be some
##benefit to caching  the matrix inverse than constantly repeating the computation.
##Here is a pair of functions that will store and calculate by creating an object
##and stores the matrix and caches the inverse of said matrix.

##This function creates the "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the matrix's inverse of above special matrix created by
## the above function. If the inverse has already been created and the orginal matrix
## has not been changed then it will go get the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}


