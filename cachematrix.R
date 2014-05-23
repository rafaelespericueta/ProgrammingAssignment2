
## This code makes use of the R language scope properties to effectively cache
## a matrix inverse, so that the time-costly matrix inverse process could be 
## avoided, at the cost of storing the matrix inverse. 

## The function makeCacheMatrix takes as input a square invertible matrix.
## It stores this matrix, as well as it's inverse (once it's been computed),
## in it's own private environment, so the memory of these matrices persists.
##
makeCacheMatrix <- function(x = matrix()) {
  # Here we set up a private environment in which to "cache" the matrix and
  # eventually its inverse matrix.
  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  
  # Return the cached version of matrix x.
  get <- function() x
  
  # Cache the freshly calculated inverse matrix.
  setinverse <- function(s) xi <<- s
  
  # Return the previously cached inverse matrix.
  # Return NULL if the inverse matrix hasn't been cached.
  getinverse <- function() xi
  
  # Return a list of the above functions, which can then be used to access
  # this private environment wherein the matrices have been cached.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve takes as input a matrix previously cached by the
## function makeCacheMatrix, and returns the inverse of that input matrix.
## If the inverse isn't yet saved in cache, it's computed, saved in cache, and
## returned, otherwise it's simply retrieved from cache and returned.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # If the inverse has been previously cached, return it.
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  # The inverse matrix wasn't cached previously. So we need to calculate it,
  # cache it for quicker access next time, and return the inverse matrix.
  data <- x$get()
  xi <- solve(data, ...)
  x$setinverse(xi)
  xi
}


##
## The following code was used to test the above routines, as well as to
## see how much faster returning a cached inverse matrix was as opposed to
## computing it.  I was surprised that the calcuated inverse was as fast as
## it was! With a 1000x1000 matrix, it took 0.42 seconds to compute the 
## inverse, while it took 0.25 secs to retrieve the cached inverse. I would 
## have thought the speed up would be far more than double, but it was less 
## than double. Matrix inverses must be computed quite efficiently in R.
##

#x <- matrix(sample(-100:100, 1000000, replace=T) , nrow=1000, ncol=1000)
#X <- makeCacheMatrix(x)

#t1 <- Sys.time()
#cacheSolve(X)
#t2 <- Sys.time()

#t3 <- Sys.time()
#cacheSolve(X)
#t4 <- Sys.time()

#print( difftime(t2, t1) )  # 0.4203792 secs
#print( difftime(t4, t3) )  # 0.2457998 secs
