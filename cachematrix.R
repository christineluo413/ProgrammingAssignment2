## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  n <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    n <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    n <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    n
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix 
## If the inverse is calculated (and the matrix  not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Use matrix multiplication to calculate the inverse
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
