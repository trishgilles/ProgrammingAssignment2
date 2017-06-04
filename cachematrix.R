## This pair of functions allows calculates the inverse of a matrix
## and caches it, so that it can be retrieved from memory without 
## having to recalcualte it
## to run call makeCacheMatrix(a), for matrix a, 
## then pas the resultant vector to cacheSolve, which will return the inverse of a.
                                                                            


## Create a list of functions to set and get a matrix and its inverse.
## Allows values to be cached and retrieved later

makeCacheMatrix <- function(x = numeric()) {

  i <- NULL
  set <- function(y) {
  x <<- y
  i <<- NULL
  }
    get <- function() x
    setinv <- function(inv) {
    i <<- inv
    message(c( "i is set", i))
    i}

  getinv <- function() i 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## this function calculates the inverse of a matrix using the solve() funtion, 
## taking a cached value where one exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i<-NULL
  i<-x$getinv()
  if(!is.null(i)) {
    message(c("getting cached data",i))
    return(i)}
  else{
    message("not getting cached data",i)
  }
  mat<-x$get()
  i<-solve(mat)
  print(i)

  x$setinv(i)
  i
}

