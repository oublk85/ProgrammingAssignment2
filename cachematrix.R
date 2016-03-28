rm(list=ls())
## Creates a special matrix that can cache its inverse, and computes the inverse

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
  
}


## This function computes the inverse of the special matrix from above

cacheSolve <- function(x, ...) {
    m <- x$getinver()
    if(!is.null(inver)) {
      message("getting cached data")
      return(inver)
    }
    data <- x$get()
    inver <- inverse(data, ...)
    x$setinver(inver)
    inver
  }


