## This function caches the inverse of a matrix to lessen the computations required

## makeCacheMatric is a function to set/get the values of the matrix and the inverse values of the martix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ##  First check if the matrix is not null then return cache value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##  Otherwise solve and return the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
