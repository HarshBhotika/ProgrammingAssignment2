## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix takes the matrix input and sets its value
## The function cacheSolve takes the output of makeCacheMatrix and computes its inverse

## Write a short comment describing this function

## Take matrix as the input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                     ## Initialize inv i.e inverse value to null
  set <- function(y) {                            ## Define set function
    x <<- y
    inv <<- NULL
  }
  get <- function() x                             ## Define get function
  setinverse <- function(inverse) inv <<- inverse ## Assign value of inverse
  getinverse <- function() inv                    ## Get the value of inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

## Compute the inverse
## If inverse has already been calculated then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {                             ## If inv is not null print message and return the value
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)                         ## Compute the inverse using solve function
  x$setinverse(inv)                               ## Set the value of inv to the calculated inverse value
  inv
}
