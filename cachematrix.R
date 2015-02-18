## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#library('MASS')
tracback()
makeCacheMatrix <- function(x = matrix()) {
  
  inverseOfx <- NULL
  set <- function(y) {
    x <<- y
    inverseOfx <<- NULL
  }
  get <-  x
  setinverse <- function(inverse) inverseOfx <<- inverse
  getinverse <- function() inverseOfx
  return list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseOfx <- x$getinverse()
  if (!is.null(inverseOfx) && (x$set == x$get)) {
        print("retreive cached inverse matrix")
    return(inverseOfx)
  } else {
    inverseOfx <- solve(x$get())
    x$setinverse(inverseOfx)
    return(inverseOfx)
  }
}

mrx <- matrix(c(2,4,5,6,7,9,4,2,8), nrow=3, ncol=3)

cacheMatrix <- makeCacheMatrix(mrx)
cacheSolve(cacheMatrix)

