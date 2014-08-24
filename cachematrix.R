## The following functions will be used to calculate matrix inverse for a square invertible matrix. 
## Since Inverse calculation is a costly computation, we are trying to create a matrix that will store the value of the 
## inverse once it is calculated. Every time the function is called, first cache values will be searched for. 
## If no value is found, or if the matrix has changed, a new value will be computed. We are relying on the normal 
## error handling behavior of R in case matrix is not solvable.

## This function will create a special matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## initialize variable for storing and retrieving the matrix inverse
  ## set function will set the initial values to the matrix to be inverted into x and inv will be initialized to NULL.
  ## This will only happen if x has not been initialized and inverse calcuted before.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get will get x if already set previously (and x has not changed)
  get <- function() x
  ## setinverse will calculate the inverse if not done so previously
  setinverse <- function(inverse) inv <<- inverse
  ## getinverse will get the preciously calculated inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will check cache for inverse of a matrix. If calculated earlier and stored in cache, the function
## will retrieve the solution from the cache. If value is not found in the cache, the function will calculate
## or solve for the inverse and store the result in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if an inverse is found in cache nad inv is not null, get that value instead onf calculating again
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if no value is found in cache, calculate inverse and store in memoryit
  data <- x$get()
  inv <- solve (data)
  x$setinverse(inv)
  inv
}
