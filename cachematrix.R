## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
