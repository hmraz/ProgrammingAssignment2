## These two functions allow the user to set a value to a matrix and then compute its inverse
## The idea is to only compute the inverse if the matrix values changes; otherwise the previous inverse is called from a cache

## This first function initializes the needed variables (and the cache variable) as well as the set and get functions
## It also defines the setinverse and getinverse functions that interact with the cache and are used by the cacheSolve function

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This functions determines the inverse of a matrix
## The function only calls the solve R function if the inverse does not already exist in the cache
## the inverse is stored in the variable m
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ## Using solve to determine the inverse
  x$setinverse(m)
  m
}


