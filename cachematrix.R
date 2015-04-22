## Inverting a matrix is a costly function and should be cached after the first time it is calculated.
##To do this you can use the functions below. First call, makeCacheMatrix passing it the matrix to be inverted. 
##The result should be passed to cacheSolve. Subsequent calls to cacheSolve will return the cached matrix.

##This function creates a list of functions that can be used to handle caching the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##This function calculates the inverse of a matrix and caches the result for subsequent calls.

cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
