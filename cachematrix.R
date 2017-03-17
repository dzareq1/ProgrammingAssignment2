## These two functions allow user to create an invertible matrix
## and cache it rather than compute it again.

## makeCacheMatrix provides list of functions that allow to store the inverse of the matrix
## which is created by cacheSolve.
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

## cacheSolve function firstly checks if inverse of the matrix has been already calculated.
## If yes, it takes inverse of matrix from cache.   
## If no, it computes inverse of the matrix using solve() function. 
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
        ## Return a matrix that is the inverse of 'x'

