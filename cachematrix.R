## Cached version of a Matrix to support efficient inversion

## Wrapper function for matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  get <- function() x
  set <- function(new_x) {
    x <<- new_x
    inv <<- NULL
  }
  get_inv <- function() inv
  set_inv <- function(new_inv) inv <<- new_inv

  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Computes cached inverse

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(is.null(inv)) {
    inv <- solve(x$get())
    x$set_inv(inv)
  }
  inv
}
