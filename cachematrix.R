makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinversa <- function(inverse) invrs <<- inverse
  getinversa <- function() invrs
  list(set=set, get=get, 
       setinversa=setinversa, getinversa=getinversa)
}



cacheSolve <- function(x, ...) {
  invrs <- x$getinversa()
  if(!is.null(invrs)) {
    message("getting cached data.")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data)
  x$setinversea(invrs)
  invrs
}






