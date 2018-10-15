## "makeCacheMatrix" creates a matrix that can cache its inverse
## "cacheSolve" computes the inverse of "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  makeInverse <- function(inverse) m <<- inverse
  grabInverse <- function() m
  list(set = set, get = get,
       makeInverse = makeInverse,
       grabInverse = grabInverse)
}




cacheSolve <- function(x, ...) {
  m <- x$grabInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$makeInverse(m)
  m
}
