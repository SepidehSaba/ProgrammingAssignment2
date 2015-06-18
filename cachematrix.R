##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

## an invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  # set the value of the matrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix 
  get <- function() x
  
  # set the the calculated inverse of matrix
  setInverse <- function(Inverse) m <<- Inverse
 
   # Get the the calculated inverse of matrix
  getInverse <- function() m
 # return a list of functions
   list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## return: inverse of the original matrix input to makeCacheMatrix()
  # reaching to the calue of invesed matrix
  m <- x$getInverse()
 
   #check if value available or being already cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if the value did not exist in cache will calculte it and set it 
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
