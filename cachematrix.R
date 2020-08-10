## Function to create a cached matrix inversion function
## Based on the README.md example of a ached mean

#The first function, `makeCacheMatrix` creates a special "matrix", which is
#really a list containing a function to
#
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix())
{
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


## Return an inverse matrix, if it was calculated before return the cached version

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("Using cached results")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}