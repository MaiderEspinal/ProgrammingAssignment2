## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. It returns a list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # 1. set the matrix
  set <- function(y) {
    x <<- y       
    inv <<- NULL
  }
  
  # 2. get the matrix
  get <- function() x
  
  # 3. set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # 4. get the inverse
  getInverse <- function() inv
  
  # Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special "matrix"  returned by makeCacheMatrix. 
## 1. If the inverse has already been calculated (and the matrix has not changed), it retrieves the inverse from the cache.
## 2. Otherwise, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inv <- x$getInverse()
  
  # 1. If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # 2. Otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse and return it
  x$setInverse(inv)
  inv
}
