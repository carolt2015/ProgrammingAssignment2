## Creates a special "matrix" object that consists a list of 'set' and 'get' functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the value of the matrix
  get <- function() x
  ## Set the value of the inverse
  setinversematrix <- function(matrix) m <<- matrix
  ## Get the value of the inverse
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {   
  m <- x$getinversematrix()
  ## If the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else
  data <- x$get()
  ## solve() computes the inverse of a square matrix 
  m <- solve(data, ...)
  x$setinversematrix(m)
  m  
}
