## makeCacheMatrix() contains a list of function to cache the inverse of the matrix given
## cachesolve() find the inverse of given matrix by first looking at the cache value

## the function contains a list of 4 functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<-NULL
  }
  get <- function () x
  setinv() <- function(inv) m <<- inv
  getinv() <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the function first check whether the inverse of the matrix has been calculated
## if yes then take value directly from the cache and skip computation.
## otherwise compute the inverse of the matrix and cache the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) return(m)
  data <- x$get()
  m <- solve(x,...)
  x$setinv(m)
  m
}
