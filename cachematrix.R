## MakeCacheMatrix create an object of two varibles - x and Inverse, and 4 functions
## Functions set() and get() allow to save matrix to variable x and get the matrix
## Function setInverse() is used to save an inverse matrix of x into variable Inverse
## Function getInverse() returns saved value of inverse matrix saved in Inverse


makeCacheMatrix <- function(x = matrix()) {

  ##Assign Null to inverse when create an object  
  Inverse <- NULL
  set <- function(y) {
    ## Saved matrix in x
    x <<- y
    ## Assign NULL to Inverse when change matrix x
    Inverse <<- NULL
  }
  
  ##Get value of matrix x
  get <- function() x
  
  ##Save inverse matrix InvM to Inverse
  setInverse <- function(InvM) Inverse <<- InvM
  
  ##Get inverse matrix from Inverse
  getInverse <- function() Inverse
  
  ##return functions as parameters of function makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function cacheSolve check whether there is a cached value of a matrix
## If not it computes and caches it 

cacheSolve <- function(x, ...) {
  
  ##Check stored value of inverse matrix of matrix x     
  Inverse <- x$getInverse()
  
  ##If there is cached value, then return it
  if(!is.null(Inverse)) {
  ##  message("getting cached data")
    return(Inverse)
  }
  
  ##Otherwise compute the inverse matrix and cache it
  dataMatrix <- x$get()
  Inverse <- solve(dataMatrix, ...)
  x$setInverse(Inverse)
  
  ## Return a matrix that is the inverse of 'x'
  Inverse
}
