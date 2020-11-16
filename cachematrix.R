## creating special matrix; the function has 5 parts
## 1. Setting the inverse of a matrix to null (initially)
## 2. Setting up the matrix data
## 3. fethcing the matrix
## 4. Setting and fetching the inverse of matrix it first search the cached 
##    inverse using lexical scoping, then returning the inverse
## 5. Return all operations in the form of a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## setting matrix data
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## fetching the matrix data
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the matrix created in makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ifss (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}