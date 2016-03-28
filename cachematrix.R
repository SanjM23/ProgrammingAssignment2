## Matrix inversion is a costly computation.
## There is benefit in chacing the inverses of a matrix instead of computing it repeatedly.

## The following function creates a list of functions that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
  
}


## Compute the inverse of the matrix returned by makeCacheMatrix()
## If the inverses has been calucalted the retrieve it from the cache

cacheSolve <- function(x, ...) {
  
          ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  ## if inverse has been calculated
  if ( ! is.null(m)) {
    ## skip the computation
    message ("getting cached data")
    return(m)
    
    }
    ## calculate the inverse
    data <- x$get()
    m <- solve (data, ...)
    x$setInverse(m)
    m
  
}
