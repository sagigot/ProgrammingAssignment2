# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

# The function returns the inverse of the matrix. 
# first it check if the inverse has already been computed. if the inverse was already computed it gets the result and
# skips computation.


cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("Loading data from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
