## The objective of this whole program is to cache the inverse operation.
## This here is accomplished with two functions primarily they are
## 1. makeCacheMatrix - This function takes a matrix x as input and returns a list 
## the list is list of funtions ( set,get,setinverse,getinverse).
## set -- sets the value of the matrix
## get -- returns the matrix value
## setinverse -- caches the calculated inverse
## getinverse -- returns the inverse from the cache
## 2. cacheSolve - This function makes use of the caching function makeCacheMatrix 
## to find the inverse of a matrix

## This helps in caching the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
 
  mk_ch <- NULL
  ## Sets the matrix value from the parameter and sets the cached value to null 
  ## whenever a new matrix value is set
  set <- function(set_val) {
    x <<- set_val
    mk_ch <<- NULL
  }
  ## Returns the matrix as it is
  get <- function() x
  
  ##Caches the inverse matrix value
  setinverse <- function(solve) mk_ch <<- solve
  
  ##Returns the cached inverse
  getinverse <- function() mk_ch
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the matrix thats set using makeCacheMatrix
##This function takes a list of functions as input, this list is produced as result of makeCacheMatrix function
##This input list is list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## Checks if the inverse matrix is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##computes the inverse
  mat <- x$get()
  m <- solve(mat, ...)
  ##caches the inverse
  x$setinverse(m)
  ##Returns the inverse
  m
}
