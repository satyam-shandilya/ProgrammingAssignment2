## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix<- NULL
  setmatrix <- function(y){
    x<<- y
    inverse_matrix<<- NULL
  }
  getmatrix<- function()x
  getinverse<- function()solve(x)
  setinverse<- function(solve) inverse_matrix<<- solve
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix<- x$getinverse()
  if(!is.null(inverse_matrix)){
    message("getting cache data")
    return(inverse_matrix)
  }
  data<- x$getmatrix()
  inverse_matrix<-solve(data,...)
  x$setinverse(inverse_matrix)
  inverse_matrix
  
}
