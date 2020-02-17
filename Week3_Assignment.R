# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv_matrix <- NULL
     set <- function(y) {
          x <<- y
          inv_matrix <<- NULL
     }
     get <- function() x
     setInverse <- function(Inverse) inv_matrix <<- Inverse
     getInverse <- function() inv_matrix
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#            If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
     inv_matrix <- x$getInverse()
     if(!is.null(inv_matrix)) {
          message("getting cached data")
          return(inv_matrix)
     }
     data <- x$get()
     inv_matrix <- solve(data, ...)
     x$setInverse(inv_matrix)
     inv_matrix
}

