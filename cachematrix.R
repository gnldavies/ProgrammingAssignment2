## R Programming - Programming Assignment 2: Lexical Scoping

## My comments are provided below, giving an overall description of what the functions do.

## The first main function, 'makeCacheMatrix' creates a matrix, containing a function to:
## - set the value of the matrix; - get the value of the matrix;
## - set the value of the inverse; - get the value of the inverse.

makeCacheMatrix <- function(x = matrix(c(1,3,2,4),nrow=2,ncol=2)) {
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

## The second main function 'cacheSolve' calculates the
## inverse of the matrix created with the above function.
## It checks to see if the inverse has already been calculated.
## If it has, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data ...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
