##  Cachematrix.R
##  Calculate the inverse of a matrix and "cache" the result (the inverse)
##  for easy retreival after first calculation. Cached values will be updated
##  when the matrix is altered   
##  

## function makeCacheMatrix()
## Brief: create a "special" list containing functions to get/set the values of
## the matrix and to get/set the inverse of the matrix
## Parameters:
## x: a square matrix that is invertible

makeCacheMatrix <- function(x = matrix()) {
  #initialise cache (no value)
  inv_c <- NULL
  
  ## function set
  ## set the values of matrix x and initialise/reset cached inverse
  ## parameters:
  ## y: a square matrix that is invertible
  set <- function(y){
    x <<- y
    inv_c <<- NULL
  }
  ## get will return the matrix x 
  get <- function() x
  ## setinverse will cache the calculated inverse matrix 
  setinverse <- function(inverse) inv_c <<- inverse
  ## getinverse returns the cached inverse if a value exists, otherwise NULL
  getinverse <- function() inv_c
  # create the list of functions:
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function cacheSolve()
## Brief: return a matrix that is the inverse of 'x'
## If the inverse exists in cache, return that value, otherwise calculate the
## inverse and store it in cache
## Parameters:
## x: a square matrix that is inversible
## ...: further arguments passed to or from other methods

cacheSolve <- function(x, ...) {
  #get the inverse value from cache (will return NULL if no cached value exists)
  inv_c <- x$getinverse()
  if(!is.null(inv_c)) {
    ## There exists a cached value, so use it:
    message("getting cached data")
    return(inv_c) #return will terminate the function early and return the cached inverse
  }
  ##This part will only be executed when inv_c is NULL (no cached inverse)
  ##get the matrix
  matrix_data <- x$get()
  ##calculate the inverse
  inv_c <- solve(x, ...)
  ##cache the inverse
  x$setinverse(inv_c)
  ##return the inverse
  return(inv_c)
}
