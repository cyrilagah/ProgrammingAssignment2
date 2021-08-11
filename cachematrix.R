## Put comments here that give an overall description of what your functions do
## The program consists of two functions which calculate and cache the inverse of a matrix

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
  
            inv <- NULL
            set <- function(y){
                    x <<- y
                    inv <<- NULL
            }
            
            get <- function()x
            setinv <- function(inverse) inv<<-inverse
            getinv <- function()inv
            list(set = set, get = get,
                 setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve is a function which calculates the inverse of the makeCacheMatrix. 
## It also retrieves the cached data when a particular matrix has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
