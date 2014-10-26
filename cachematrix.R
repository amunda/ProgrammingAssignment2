## These functions will allow you to cache expensive operation of
## calculating matrix inverse. These calculations can be really expensive when the vector size is huge
## We use lexical scoping of R to implement caching.

## This is a special matrix object which can be cacheable. It stores both the original and cached version of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This method first checks if inversed matrix is preesent in cache or not. 
## if it isn't present in cache then it performs the inversion of matrix and then returns the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    message("Using cached version of inverse matrix")
    return(inverseMatrix)
  }
  
  matrix <- x$get()
  inverseMatrix <- solve(matrix,...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
