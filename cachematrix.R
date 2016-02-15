## Put comments here that give an overall description of what your
## functions do

## We are creating a special "matrix" object that will have 4 functions allowing us return an inverse matrix after the first inverse operation 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) inv <<- solve
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This Function is called to get the inverse of the special "Matrix" object, it checks and returns an existing inverse 
## or else complutes the inverse first time and updated the special matrix object with the computed inverse 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting Inverse Matrix")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  

  x$setInverse(inv)
  inv 

  ## Return a matrix that is the inverse of 'x'
  ## To test 
  ##   y = makeCacheMatrix( matrix(1:4,2,2))
  ## cacheSolve(y)
  ##        [,1] [,2]
  ## [1,]   -2  1.5
  ## [2,]    1 -0.5
  ## cacheSolve(y)
  ## getting Inverse Matrix
  ##      [,1] [,2]
  ## [1,]   -2  1.5
  ## [2,]    1 -0.5
  ##   
  ## 
  ## 
}
