## This function will create a cacheable matrix with the
## 'makeCacheMatrix' function.  It has all of the setters and
## getters for inital values of the matrix (set & get) and 
## set/get the cacheable inversion (setInverse/getInverse)

## Function for setting/getting the makeCacheMatrix object properties
## @args - matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## 'cacheSolve' will invert a matrix that was stored via 'makeCacheMatrix'
## First checks if matrix inversion performed.  If cached, will load cached
## inversion.  If not cached, will perform 'solve()' on matrix and cache the
## inversion
## @args - makeCacheMatrix object

cacheSolve <- function(x) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}