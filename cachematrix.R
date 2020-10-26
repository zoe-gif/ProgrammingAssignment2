## inverse matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolv <- function (solv) m <<- solv
  getsolv <- function () m
  list(set=set, get=get, setsolv=setsolv, getsolv=getsolv)
}


## This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolv()
  if (!is.null(m)){
    return(m)
  }
  dat <- x$get()
  m <- solve(dat, ...)
  x$setsolv(m)
  m
}
