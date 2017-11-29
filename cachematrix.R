## First of all you want this formula to work with any matrix, so I created a matrix for myself to see
## m <- matrix(c(4, 3, 2, 1), nrow = 2, ncol = 2, byrow = TRUE)
# Now first you need to set/get the value and later set/get the inverse of the matrix
# Why would you cache the matrix? Saves a lot op RAMs
# The function for the matrix should be: Afterwards cache the inverse we have to write #Then set the function for y 
#Then set, use <<- because R needs to search not in global environment, but local
#Then get the function and set the inverse #Now we need to solve the cached matrix, 
#if it is not calculated it should retrieve the inverse of the data.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

