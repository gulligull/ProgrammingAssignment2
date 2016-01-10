## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will 
## not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # initially set i (inverse) to NULL
  i <- NULL
  
  # set matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # set inverse matrix
  setinverse <- function (inverse) i <<- inverse
  
  # get inverse matrix
  getinverse <- function() i
  # put that in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get inverse matrix from previous function (if existing)
  i <- x$getinverse()
  
  # if an inverse matrix is found (i.e. i is not null) indicate that cached data is used
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if no inverse matrix is found, get the matrix
  data <- x$get()
  
  # compute the inverse matrix
  i <- solve(data, ...)
  
  # store the result for later retrieval
  x$setinverse(i)
  
  # output
  i
}
