makeCacheMatrix <- function(m_in = matrix()) {
    #Creating a NULL Matrix
    m <- NULL

    #Exporting the Matrix  
  matrix_set <- function(m_out) {
          m_in <<- m_out
          m <<- NULL
  }
  
  #Defining the return matrix 
  matrix_get <- function() m_in

  #Getting the Inverse Matrix
  matrix_set_Inverse <- function(inverse) m <<- inverse
  matrix_get_inverse <- function() m
  
  #Creating the list of objects 
  list(matrix_set = matrix_set,
       matrix_get = matrix_get,
       matrix_set_Inverse = matrix_set_Inverse,
       matrix_get_inverse = matrix_get_inverse)
}

cacheSolve <- function(m_in, ...) {
  
  # Setting Inverted Matrix 
  m <- m_in$matrix_get_inverse()
  #Checking if the matrix was already computed
  if (!is.null(m)) {
          message("Getting cached Matrix")
          return(m)
  }
  data <- m_in$matrix_get()
  m <- solve(data, ...)
  m_in$matrix_set_Inverse(m)
  m
