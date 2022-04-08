## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  This function defines the set, get, setinverse, and getinverse functions which is
#  needed to check in cache if the inverse of matrix has already been calculated 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#  This function checks n cache if the inverse of matrix has already been calculated. 
#  If not in cache, the inverse will be calculated and stored in the cache 
#  We call this function as following 
#   cacheSolve(makeCacheMatrix(matrix(data = 10:13, nrow = 2, ncol = 2)))

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
