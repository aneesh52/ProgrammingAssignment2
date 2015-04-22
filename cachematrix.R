## makecacheMatrix creates a matrix object and caches it's inverse.
## cacheSolve calculates the inverse of the above matrix
## in case the inverse has already been calculated then it returns
## from the cache instead of re-calculating.

## makecacheMatrix creates a matrix object and caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## cacheSolve calculates the inverse of the above matrix
## in case the inverse has already been calculated then it returns
## from the cache instead of re-calculating.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)){
    message("getting cached data")
    return(inv_matrix)
  }
  inv_matrix <- solve(x$get())
  x$setinverse(inv_matrix)
  inv_matrix
}