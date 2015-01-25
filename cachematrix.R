## The function makeCacheMatrix will create 
##a special matrix that can cache its inverse
## The cacheSolve function computes the inverse
## returned by makeCacheMatrix function

## makeCacheMarix will create a special matrix
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve computes the inverse of the
## special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        
}
