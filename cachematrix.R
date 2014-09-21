## The following two function are set to calculate the inverse of a given matrix and 
## if the matrix has been already calculates, it skips the computation and returns the
## inverse. The first fuction creates a list which gives us the following four elements:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## The second fuction applies the cache to skip the first step, if and only if 
## the matrix and its inverse has been already calculated.

## the makeCacheMatrix function calculates the four elements when the matrix is submited,

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}

## The function cacheSolve skips the computation and returns the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
