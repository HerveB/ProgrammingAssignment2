## CacheSovle() is an optmized version of solve() that can be used when the inverted matrix 
## of a given matrix is re-used multiple times.
## CacheSolve caches the result of solve() the first time the inverted matrix is calculated
## and returns the cached result instead of recalculating it when subsequently called

## Usage:
## Use makeCacheMatrix() to initalize the objects that will containt the matrix and eventually the cached inverted matrix
## example > f = makeCacheMatrix(test_matrix)
## Use CacheSolve() on the special object instead instead of solv()
## example > cacheSolve(f)
## repeat cacheSolve(f) and you will see the "getting cached data" message

## Example of invertible matrix that can be used for testing
test_matrix <- matrix(c(2,2,3,2),2,2)


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## x : matrix for which a set of functions will be create to a

makeCacheMatrix <- function(x = matrix()) {
  
  # i will be used to store the inverted matrix of x
  i <- NULL

  ## Create the function to set the matrix
  set <- function(y) {
    ## Assign values to the matrix x and i in the parent environment
    x <<- y
    i <<- NULL
  }
  
  ## Create the function to get the matrix value
  get <- function() x
  
  ## Create the function to set the inverted matrix
  setinv <- function(inv) i <<- inv
  
  ## Create the function to get the inverted matrix
  getinv <- function() i
  
  ## Return a list will the functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## x : special "matrix" previously created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## Retrieve the cached inverted matrix or NULL if not set yet
  i <- x$getinv()
  if(!is.null(i)) {
    ## The inverted matrix was already calculated and cached.
    ## Cached matrix is returned and a message is displayed
    message("getting cached data")
    return(i)
  }
  
  ## The inverted matrix was NOT already calculated and cached.
  ## The matrix m is retrieved
  m <-x$get()
  ## The inverted matrix of m is calculted and stored in i
  ## An error will be generated if matrix m is not invertible
  i <- solve(m, ...)
  ## Cache the inverted matrix i and return i
  x$setinv(i)
  i
}

