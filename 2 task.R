cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}