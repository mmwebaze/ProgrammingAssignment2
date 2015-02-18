## creates a special "matrix" object that can cache its inverse in a list

makeCacheMatrix <- function(x = matrix()) {
  
  inverseOfx <- NULL
  ## set the value of the Matrix
  set <- function(y) {
    x <<- y
    inverseOfx <<- NULL
  }
  ## get the value of the Matrix X
  get <- function() x
  ## Set the inverse of the Matrix
  setinverse <- function(inverse) inverseOfx <<- inverse
  
  ## get the inverse of the Matrix
  getinverse <- function() inverseOfx
  return list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inverseOfx <- x$getinverse() ## gets the inverse of the matrix stored in the list object
  
  ##checks if set matrix and matrix saved in list are the same and inverse of matrix was exists
  if ((x$set == x$get) && !is.null(inverseOfx)) {
    print("retrieve cached inverse matrix")
    return(inverseOfx)
  } else {
    inverseOfx <- solve(x$get())
    x$setinverse(inverseOfx)
    return(inverseOfx)
  }
}

#mrx <- matrix(c(2,4,5,6,7,9,4,2,8), nrow=3, ncol=3)

#cacheMatrix <- makeCacheMatrix(mrx)
#cacheSolve(cacheMatrix)