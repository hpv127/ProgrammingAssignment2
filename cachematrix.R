# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) matinv <<- inv
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function checks if the inverse needs to be calculated. If need be it calculates the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
}
