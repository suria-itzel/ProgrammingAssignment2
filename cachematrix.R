## The objective is to create two functions that reduce the computation time
## of getting the inverse of a matrix by storing the result in the cache.
## This allows us to retrieve the inverse without having to redo the computation. 


## This function creates a special matrix object that can cache its inverse.
## It does so by naming the elements in a list. If it were a "normal" matrix,
## we would get an error in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(z) {
  x <<- z
  inv <<- NULL
}
get <- function () x
setinv <- function(solve) inv <<- solve
getinv <- function() inv
list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of the matrix object created in makeCacheMatrix.
## If the inverse has already been computed for the matrix, it retrieves the result
## from the cache memory. As this function is the one that computes the inverse,
## the makeCacheMatrix function is incomplete without it. 

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if (!is.null(inv)) {
     message("getting cache data...")
     return(inv)
   }
   else {
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
   }
}
