makeCachematrix <- function(x = matrix(2,2)) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
   setsolve <- function(solve) i <<- solve
   getsolve <- function()i 
 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
cachesolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
