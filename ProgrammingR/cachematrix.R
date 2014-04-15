## These functions allows compute the inverse matrix only ones and cached the result
## in the special "matrix" object. 
## Basic Using: 
## x<-makeCacheMatrix(matrix(c(2,-1,1,2),2,2))
## cacheSolve(x)
## x$get()
## x$getInverse()

## The function makeCacheMatrix create a special "matrix" object
## Input: matrix
## Output: special matrix object which has following function
##         get, set matrix; 
##         getInverse, setInverse matrix 
makeCacheMatrix <- function(x = matrix()) {
  # im - cached inverse matrix
  im <- NULL
  # local copy of matrix
  x <- x
  # set - if matrix is changed remove cached inverse matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inv_matrix) im <<- inv_matrix
  getInverse <- function() im
  # list of available function
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve computes the first time inverse matrix, 
## if it was computed before it returns cached value.

cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  #test if cached inverse matrix exists
  if(!is.null(im)) {
    message("getting cached data")
    #return cached inverse matrix
    return(im)
  }
  #cached inverse matrix does not exist, compute it and store it
  data <- x$get()
  im <- solve(data)
  x$setInverse(im)
  im
}
