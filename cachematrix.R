## Create an object which wraps a matrix giving it some extra
# methods allowing it to cache its inverse solution.
#
# example usage: 
# M <- makeCacheMatrix(matrix(c(1:4), nrow=2, ncol=2))
makeCacheMatrix <- function(x = matrix()) {
  # cached inverse (calcuated as required so set to null initially)
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() {
    x
  }
  
  # save the inverse solution to the cache
  setSolve <- function(inverse) {
    inv <<- inverse
  }
  
  # get the inverse solution from the cache
  getSolve <- function() {
    inv
  }
  
  # return a list of functions which can be used on this object
  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


##
# finds the inverse of a cacheMatrix object. If the inverse has already
# been calculated that will be returned, otherwise calculate the 
# solution, save it to the cacheMatrix, and return the solution. 
#
# example usage: 
# M <- makeCacheMatrix(matrix(c(1:4), nrow=2, ncol=2))
# cacheSolve(M) # calculates the inverse of the matrix
# cacheSolve(M) # no calculation required - value returned from cache
cacheSolve <- function(x, ...) {
  # try getting the inverse
  inverse <- x$getSolve()
  
  # if it's null, calculate it and save it to the cachematrix
  if(is.null(inverse)) {
    message("stand by... calculating the inverse of the matrix and caching it")
    data <- x$get()
    inverse <- solve(data, ...)
    x$setSolve(inverse)
  }

  inverse
}
