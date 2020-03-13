## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  j <- NULL
  ## Method to set the matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  ## Method the get the matrix
  get <- function(){
    x
    ## Return the matrix
    }
   ## Method to set the inverse of the matrix
  setInverse <- function(inverse) j <<- inverse
  ## Method to get the inverse of the matrix
  getInverse <- function() j
   ## Return a list of the methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve<-function(x,...){
   ## Return a matrix that is the inverse of 'x'
  j<-x$getInverse()
  ## Just return the inverse if its already set
  if(!is.null(j)){
    message("getting cache data")
    return(j)
  }
   ## Get the matrix from our object
  mat<-x$get()
  ## Calculate the inverse using matrix multiplication
  j<-solve(mat,...)
  ## Set the inverse to the object
  x$setInverse(j)
  ##Return the matrix
  j
}
