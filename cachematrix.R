# Similar to the example given to us by Mr. Peng! The first function takes in an input and makes a list
# The list has 4 elements
# First one sets the value of the matrix
# Second one gets the value of the matrix
# Third sets the inverse of the matrix
# Fourth gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function checks if the inverse has been calculated already and if not it calculates it
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

