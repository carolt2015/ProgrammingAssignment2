## Creates a special "matrix" object that consists a list of 'set' and 'get' functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
   ##get the value of the matrix
  get <- function() x
  ## set the value of the inverse
  setmatrix <- function(matrix) m <<- matrix
  ## get the value of the inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {   
  m <- x$getmatrix()
  ## if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## else
  data <- x$get()
  ## solve() computes the inverse of a square matrix 
  m <- solve(data, ...)
  x$setmatrix(m)
  m  
}
