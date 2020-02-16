## These two functions exist to speed up the process of repeatedly accessing 
## the inverse of a matrix. They allow a matrix's inverse to be cached the first
## time it is calculated, and simply be repeated from the cache on supsequent 
## calls as necessary. Use by passing the matrix to makeCacheMatrix and storing
## the result in an object in the global environment (it should output a list of
## length 4), and then passing that object to cacheSolve whenever the stored
## matrix's inverse is needed.

## makeCacheMatrix takes a matrix as its one formal argument and defines an
## environment containing the matrix, a null value for its inverse, and four
## functions for manipulating the matrix:
##    set() assigns the input argument (called y) to the x object in the environment
##    and sets the inv placeholder to null to remove the results of any previous
##    runs of cacheSolve
##    get() returns the value of x stored in the makeCacheMatrix 
##    environment
##    setinv() sets the value of inv in the makeCacheMatrix environment
##    getinv(), like get(), returns the value of inv stored in the 
##    makeCacheMatrix environment.
## These four functions, as well as the values x and inv stored in the 
## makeCacheMatrix environment, allow cacheSolve to take a matrix, check
## whether its inverse is cached, and either return the cached value if it is
## or recalculate the inverse if it is not. the four functions are outputted
## as a list, with each element being named (set() is the first element and
## named 'set', etc.)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes an object of type makeCacheMatrix(), the output of the
## function above, and uses the functions defined therein to either repeat
## the cached value of the inverse of the matrix originally passed to 
## makeCacheMatrix, or if there is no cached value to calculate it from scratch.

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #recovers the value of inv stored in the makeCacheMatrix() object
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } #if there is a non-null value there, returns the existing value
  data <- x$get() #if not, cacheSolve gets the original matrix stored in makeCacheMatrix()
  inv <- solve(data, ...) #then calculates its inverse
  x$setinv(inv) #and stores that value back in the makeCacheMatrix() environment for next time
  inv #finally, it prints the inverse value
}
