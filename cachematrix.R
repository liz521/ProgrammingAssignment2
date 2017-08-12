## Caching the Inverse of a Matrix
## assume that the matrix supplied is always invertible

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


##  computes the inverse of the special "matrix" returned by function above
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}

##  application example####
#my.matrix<-matrix(1:4,2,2)
#cashed<-makeCacheMatrix(my.matrix)
#cacheSolve(cashed)
#cacheSolve(cashed)
