## github
## 
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to
## 1. set the value of the matrix - set()
## 2. get the value of the matrix - get()
## 3. set the value of the inverse of the matrix -setInverse()
## 4. get the value of the inverse of the matrix - getInverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(mat) {
      x <<- mat
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" which created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function from makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
  }
  else
  {
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  }
  return(inv)
}
