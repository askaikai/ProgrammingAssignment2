## these functions could be used to cache and/or calculate the inverse of an input matrix.
## if the inverse is already in the memory, the fxn returns the inverse from there, 
## but if no in the memory, it computes the inverse of the input matrix. 
## 
## history
## 04/20/14 ai wrote based on the examples on: https://class.coursera.org/rprog-002/human_grading/view/courses/972078/assessments/3/submissions

## this fxn creates the object that can cache the input matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this fxn computes the inverse of the input matrix, based on the object created 
## in makeCacheMatrix. 
## if the inverse has already been calculted, then this fxn should retrieve it from
## the cache.
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse() # does it exist in cache? 
  if(!is.null(m)) {
    message("getting cached data") 
    return(m) # if it does exist, look no further. just retun this value
  }
  data <- x$get() # if not cached...
  m <- solve(data, ...) # calculate here and ...
  x$setInverse(m) # save the result in x's cache
  m
}
