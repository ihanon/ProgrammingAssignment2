## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix builds a creates a special object, which is really a list containing a function to
## 1. Set the value of a matrix
## 2. Get the value of a matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix

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


## Write a short comment describing this function
## cacheSolve function inverse a matrix object created by makeCacheMatrix
## First it checks whether the inverse is already stored in the memory or not
## It solves any matrix for only the first time. Next times it just fetches from the memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
