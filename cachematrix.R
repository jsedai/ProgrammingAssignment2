## The following functions calculate the inverse of square invertible matrix.
## For example, if X is a square invertible matrix, then it calculates 
## its inverse with the help of solve(X) function in R.
## It calculates the inverse of a matrix and saves it to the cache
## such that the next time when user attempts to calculate the matrix inverse,
## previously saved value is returned instead of repeating the calculation.


## This function creates a special "matrix" object that can cache its inverse.
## It contains functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. It first checks  if the inverse has already  been
## calculated (and the matrix has not changed), then the it retrieves the inverse
## from the cache and skips the computation.Otherwise, it calculates the matrix inverse
## with the help of 'solve()' function and sets the value of the inverse 
## in the cache via the 'setinverse()' function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## Return a matrix 'i' that is the inverse of 'x'
  return(i) ##or simply i works
}
