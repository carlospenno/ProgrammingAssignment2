## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverted.matrix <- NULL
  set <- function(y) {
    x <<- y
   inverted.matrix <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inverted.matrix <<- inverse
  get.inverse <- function() inverted.matrix
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inverted.matrix <- x$get.inverse()
  if(!is.null(inverted.matrix)) {
    message("getting inverted matrix")
    return(inverted.matrix)
  }
  data <- x$get()
  inverted.matrix <- solve(data, ...)
  x$set.inverse(inverted.matrix)
  inverted.matrix
}
}
