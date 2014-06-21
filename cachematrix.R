## makeCacheMatrix creates a special matrix object. 
## cacheSolve inverts the matrix.
## If the inverse of the matrix already has been calculated, it will 
## find it in the cache and return it without calculating it again.

makeCacheMatrix <- function(x=matrix()) {
      
      # stores the cached inverse matrix
      cacheinverse <- NULL
      
      # sets the matrix
      set <- function(y) {
            x <<- y
            cacheinverse <<- NULL
      }
      
      # gets the matrix
      get <- function() x
      
      # sets the inverse
      setinverse <- function(inverse) cacheinverse <<- inverse
      
      # gets the inverse
      getinverse <- function() cacheinverse
      
      # returns the matrix with defined functions
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



# cacheSolve: Gives out the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x=matrix(), ...) {
      cacheinverse <- x$getinverse()
      
      # If the inverse is already calculated, it returns it
      if (!is.null(cacheinverse)) {
            message("getting cached data")
            return(cacheinverse)
      }
      
      # If the inverse is not yet calculated, it will be calculated now
      data <- x$get()
      cacheinverse <- solve(data, ...)
      
      # Cache the inverse
      x$setinverse(cacheinverse)
      
      # Return the cached inverse
      cacheinverse
}