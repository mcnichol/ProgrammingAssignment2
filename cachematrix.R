## This function will create a cacheable matrix with the
## 'makeCacheMatrix' function.  It has all of the setters and
## getters for inital values of the matrix (set & get) and 
## set/get the cacheable inversion (setInverse/getInverse)

## Function for setting/getting the makeCacheMatrix object properties
## @args - matrix

makeCacheMatrix <- function(x = matrix()) {myCacheMatrixObject$

  ## Start with a clean value that has been emptied.
  ## remove unwanted data, or memory address data that could be living there
  m <- NULL
  
  ## If the set function is called on the makeCacheMatrix object,
  ## it will be passed a value 'y' e.g.: 'myObject$set(y)' where 'y' 
  ## will be a matrix
  ##
  ## Do this if you want to change the values in the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## An inline function call to grab the matrix object 'X' 
  ## Same as saying 'get <- function() { x }'
  ##
  ## Do this if you want to dump the matrix out to the screen 
  ## or use what's currently there e.g.: someValue <- myObject$get()
  get <- function() x


  ## This is heavily used by the cacheSolve function.  When 
  ## it wants to run the solve() function, it will use setInverse to 
  ## cache the result.  The '<<' superassignment operator is used to 
  ## lexically find the variable 'm' and give it the value of 'inverse'
  ##
  ## If you are unfamiliar with that verbage, it basically means it will step
  ## up each enclosure in a hierarchical manner until it either finds the variable
  ## 'm', or it gets to the global environment and creates a variable 'm' and assigns
  ## it the value of inverse.  In this case 'm' is one level up (Initialized to NULL)
  setInverse <- function(inverse) m <<- inverse
  
  
  ## This will grab the variable 'm'.  If setInverse has been run and all
  ## conditions are assumed valid, then it will send back the cached 
  ## inverted matrix, otherwise, it will return NULL allowing the calling 
  ## function to recognize nothing is cached and run the 'setInverse
  getInverse <- function() m
  
  
  ## This is how we can access the functions from outside of makeCacheMatrix()
  ## We will create a list with the values set, get, setInverse, getInverse
  ## Then we assign them the corresponding functions from inside the 
  ## makeCacheMatrix function
  ##
  ## Now the functions can be run by calling the function off of your 
  ## makeCacheMatrix object e.g.: myObject$getInverse()
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
  ## This calls the getInverse() function off of the matrix object that
  ## has been passed and stores it in the variable 'm'
  m <- x$getInverse()
  
  ## This if statement checks if the data returned from the getInverse() function 
  ## is NOT NULL.  If TRUE, it means that the setInverse function has been run on 
  ## this matrix and it can utilize the cached data.  It will then break out of 
  ## the function using the 'return' statement and returning 'm'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If there is no data returned from the getInverse() function, then the data has
  ## not been cached.
  ##
  ## It will subsequently grab the matrix
  data <- x$get()
  ## invert the matrix (Using 'solve()' function) and pass that to myObject$setInverse()
  x$setInverse(solve(data))
  ## finally, displaying the inverse
  x$getInverse()
}
