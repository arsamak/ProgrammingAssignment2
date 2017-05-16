## Calculating the inverse of a matrix or
## retrieving the inverse from the cache

## Function to create an invertible Matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
 set <- function(y) {
    x <<- y
    i <<- NULL
  }
 get <- function() x
 setinverse <- function(inverse) i <<- inverse
 getinverse <- function() i
 list(set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## Calculates the inverse after verifying first whether the inverse is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
