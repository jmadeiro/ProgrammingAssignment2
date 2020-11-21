## cachematrix computes the inverse of the matrix using a internal cache
## Preconditions: the matrix supplied is invertible

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set the value of the matrix
  set  <- function() {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function()
    x
  
  ## Set the inverse of the matrix
  setinverse  <- function(inverse)
    inv <<- inverse
  
  ## Get the inverse of the matrix
  getinverse <- function()
    inv
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## If the has already been calculated, take de inverse from cache
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

