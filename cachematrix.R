## makeCacheMatrix is a function that creates a matrix that can cache its own inverse as an object
## cacheSolve either retreives the cached inverse of the matrix from makeCacheMatrix 
## or calculates the inverse if it is not cached 

## makeCacheMatrix: Creates a special object that is a matrix 
## contains functions used by cacheSolve to get or set inverted values of the matrix in a cache

makeCacheMatrix <- function (x = matrix()) {

  inv<- NULL
  ##  Set value of matrix
  set<- function (y){
    x<<-y
    inv<<- NULL
  }
  ## Get the value of the matrix
  get <- function () x
  ## Set the value of the inverse of the matrix and store in cache
  setInverse <- function (invert) inv<<- invert
  ## get inverted matrix values from cache
  getInverse <- function() inv
  
  ## put functions into working environment
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve : Checks to see if the inverse of the matrix defined by the above function has already been calculate
##If it has been calculated and cached the function just retrieves the inverse from the cache
## If the inverse has not been calculated, the function calculates the inverse of the special matrix defined above using solve
## and sets the value of the inverse in the cache using the setInverse function

cacheSolve <- function(x, ...) {
  inv<-x$getInverse ()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setInverse(inv)
  inv
} 

