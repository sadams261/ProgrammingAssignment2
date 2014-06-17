## makeCacheMatrix generates a list of functions, allowing the inverse of a matrix to be cached
## cacheSolve uses this list to invert a matrix

## application scenario
## B <- makeCacheMatrix() #establishes functions
## test_matrix 
## b_set <- B$set(diag(10,5,5))  ## use set to create a diagonal matrix
## cacheSolve(B) ## returns inverse; no message
## cacheSolve(B) ## returns inverse, with message(using cached data)
## c_set <- B$set(diag(2,5,5)) ## new test matrix
## cacheSolve(B) ## returns invers, no message



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #define functions. 
  get <- function() x
  
  #Note: assumes that Solve can be used (matrix is invertible/nonsingular)
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  #return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## CacheSolve returns a matrix that is the inverse of x. It uses cached data if available
## when cached data is used, print a message 
## otherwise, invert matrix and cache the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
