## The following functions create a special matrix object and cache its inverse.


## Given a matrix, this function returns a special matrix object that has accessors and modifiers for the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Given a special matrix object created by the makeCacheMatrix, this function either returns the inverse of the matrix if chached or calculates it,  caches it in the special matrix object and then returns the calculated value.
cacheSolve <- function(x, ...) {
       
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
