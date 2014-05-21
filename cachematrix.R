## Written by virtualmorgan on 5/21/2014 based on the homework assignment
## for the R Programming (v. 003) course.

## The point behind these functions is to highlight lexical scoping for a function.
## The code also provides good practice for using GitHub to push and pull code.


## The makeCacheMatrix function creates a special "matrix" object that can cache its 
## inverse. Essentially, it is a list that contains references to a set of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, 
       get=get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
