## makeCacheMatrix
##makeCacheMatrix do

## step1

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) z <<- inverse
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}




##  step2

cacheSolve <- function(x, ...) {
  ## Returning matrix
  z <- x$getInverse()
  if(!is.null(z)){
    message(" cached data was got")
    return(z)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(z)
  j
}