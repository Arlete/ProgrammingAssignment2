
#The next two functions allow to cache the inverse of a matrix.
#The first function "makeCacheMatrix"creates a special matrix object that can cache its inverse.
#The second function "cacheSolve" computes the inverse of the special matrix returned by the "makeCacheMatrix" function.



# This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL 

set <- function(y){
  x <<- y
  inv <<- NULL
}

get <- function() x
setInverse <- function(solve) inv <<- solve
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


# This function computes the inverse of the matrix returned by the "makeCacheMatrix" funtion above.
# Note: If the inverse has already been calculated (and the matrix has not changed), then this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv  
  
}
