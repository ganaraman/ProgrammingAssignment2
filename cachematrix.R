## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

# makeCacheMatrix function creates a special matrix which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of input matrix
# get the value of the inverse of input matrix
makeCacheMatrix <- function(x = matrix()) {
  val <- NULL
  set <- function(y) {
    x <<- y
    val <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) val <<- inverse
  getinverse <- function() val
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve:  function calculates the inverse of the special "matrix" created by makeCacheMatrix.
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and 
# sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   val <- x$getinverse()
  if(!is.null(val)) {
    message("getting cached data")
    return(val)
  }
  data <- x$get()
  val <- solve(data, ...)
  x$setinverse(val)
  val
}
