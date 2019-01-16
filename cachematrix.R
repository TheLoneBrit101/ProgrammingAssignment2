## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialising the inverse property
  m <- NULL
  
  ## Setting the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  ## Method to set the inverse of the matrix
  setsolve <- function(solve) m <<- solve
  
  ## Method to get the inverse of the matrix
  getsolve <- function() m
  
  ## Return a list of the methods
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## Return the inverse if its already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculating the inverse
  m <- solve(data)
  
  ## Setting the inverse to the object
  x$setsolve(m)
  
  ## Returning the matrix
  m
}
