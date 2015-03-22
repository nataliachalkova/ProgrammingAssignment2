## set and get the value of matrix
## set and get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- (inverse)
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


#сalculates the inverse of the special "matrix"
#if matrix contains in cahche, it returns cached value of inverse of the matrix
#and skips the computation.
cacheSolve <- function(x, ...) {  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}