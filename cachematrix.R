## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.
## For this assignment, I'm assuming that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to 
## 1. set a mtrix
## 2. get a matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    ## When a new matrix is assigned, we need to clear the cached inverse matrix
    inv <<- NULL
  }
  
  ## Return the stored matrix
  get <- function() x
  
  ## Cache the inverse which is computed and assigned
  setinverse <- function(inverse) inv <<- inverse
  
  ## Return the cached inverse
  getinverse <- function() inv
  
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## If the inverse is already available in cache, we return the cached inverse
  ## The return statement ensures that the rest of the computation is not performed if we can return the cached result
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## If the cached result is not available, we go ahead and solve for the inverse.
  data <- x$get()
  inv <- solve(data)
  
  ## After solving the matrix inversion, cache the result
  x$setinverse(inv)
  inv
}
