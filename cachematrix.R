## Matrix inversion is usually a costly computation and there are benefits to caching the inverse 
## of a matrix rather than compute it repeatedly. The following pair of functions cache the
# inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # initialize 'x' as a matrix
  i <- NULL                                 # initialize 'i' (for inverse)
  set <- function(y) {
    x <<- y                                 # assign 'y' (input argument) to 'x' in the parent environment
    i <<- NULL                              # assign NULL to 'i' in the parent environment. Clears any value 
                                            # of 'i' that had been cached by a prior execution of cacheSolve().
  }
  get <- function() x                       # Since the symbol 'x' is not defined within get(), R retrieves it
                                            # from the parent environment of makeCacheMatrix().
  setinverse <- function(inverse) i <<- inverse # assign inverse to 'i' in the parent environment
  getinverse <- function() i
  list(set = set,              # gives the name 'set' to the set() function defined above
      get = get,               # gives the name 'get' to the get() function defined above
      setinverse = setinverse, # gives the name 'setinverse' to the setinverse() function defined above
      getinverse = getinverse) # gives the name 'getinverse' to the getinverse() function defined above
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache. Returns a matrix 'i' that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()          # get inverse and store to 'i'
  if(!is.null(i)) {            # check if inverse matrix is already in cache ('i' will be NULL)
    message("getting cached data")
    return(i)
  }
  data <- x$get()              # get matrix to calculate inverse of
  i <- solve(data, ...)        # calculate inverse of matrix and store to 'i'
  x$setinverse(i)              # set inverse
  i
  
}