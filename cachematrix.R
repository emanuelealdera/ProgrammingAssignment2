#this function creates a "matrix" object, containing the matrix x itself and, if cached,
#it can contain its inverse. If the value is not cached, however, the inverse
#will be set to NULL
#it return a list of functions that can be used to set/get the matrix itself and 
#its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  get_inverse <- function() inverse
  list(set = set, get = get, get_inverse = get_inverse, set_inverse = set_inverse)
}
## This functions solves the inverse for the matrix created with the function above
# If the value of the inverse is already cached, it retrieves that information, 
#else it calculates it and stores in cache

cacheSolve <- function(x) {
  inverse <- x$get_inverse()
  if (!is.null(inverse)){ 
    #we have the inverse in cache
    message("Getting the inverse matrix using cache")
    return(inverse)
  } 
  else {
    #we create a cacheMatrix, calculate the inverse and
    #we store the result in cache
    matrix <- x$get()
    inverse <- solve(matrix)
    x$set_inverse(inverse)
    inverse
  }
}
