## These two functions allow user to create an inverse of the matrix
## and store it rather than compute it again.

## makeCacheMatrix provides list of functions that allow to get and store the inverse of the matrix
## which is created by cacheSolve function.
## Moreover, they make it available in case of recalculation of the same data.

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  set_inv_mat <- function(solve) inv_mat <<- solve
  get_inv_mat <- function() inv_mat
  list(set = set, get = get,
       set_inv_mat = set_inv_mat,
       get_inv_mat = get_inv_mat)
}

## cacheSolve function firstly checks if inversed matrix has already been calculated.
## If so, it takes stored result from cache and skip recalculations.   
## If inversed matrix is not available, the function computes it using solve() function.
    
cacheSolve <- function(x, ...) {
  inv_mat <- x$get_inv_mat() 
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$set_inv_mat(inv_mat)
  inv_mat
}
       

