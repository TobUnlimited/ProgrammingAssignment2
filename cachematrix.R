# Matrix inversion is usually a costly computation and the following two functions create 
# the possibility to cache the result of a matrix inversion operation, in an attempt to 
# save computing resources where possible.


# Returns a list containing a function to:
# -set the value of the matrix
# -get the value of the matrix
# -set the value of the inverse of the matrix
# -get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solv) s <<- solv
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


# Return a matrix that is the inverse of 'x'
# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
# the value of the inverse in the cache via the setSolve function.
cacheSolve <- function(x, ...) {
  
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}


# This is a test function to show the caching functionality in use.
test_cacheSolve <- function() {
  
  simpleMatrix <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)
  #print("Start:")
  #print(simpleMatrix)
  madeMatrix <- makeCacheMatrix(simpleMatrix)
  
  # Get inverse first time.
  resInv1 <- cacheSolve(madeMatrix)
  #print("Res 1 exec'd")
  #print(resInv1)
  
  # Get inverse second time, i.e. get the cached inverse.
  resInv2 <- cacheSolve(madeMatrix)
  #print("Res 2 exec'd")
  #print(resInv2)
}
