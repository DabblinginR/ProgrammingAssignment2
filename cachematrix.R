#makeCacheMatrix will calculate the inverse of a matrix and cache it.

makeCacheMatrix <- function(x = numeric()) {
  # start by shameless copying of the provided code for caching a mean value
    m <- NULL # clean up m in case there is an old value there.
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve #solve() is the R function to find the inverse of a matrix
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) # having our last line as a list means our output will be all four of these things, which is necessary to feed them into the second function.
}


#cacheSolve will return the inverse of the matrix... drawing from the cache if possible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinv()
    if(!is.null(m)) { # This is where we check if we already calculated the inverse.
      message("getting cached data")
      return(m) #using the return function will terminate the cacheSolve function here. So the rest of the function assumes it has to calculate.
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
