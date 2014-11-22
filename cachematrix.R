# makeCacheMatrix creates a special matrix that can cache it's inverse
# cacheSolve sets the inverse of the special matrix in cache

# makeCacheMatrix creates a special matrix that allows it's inverse to be
# chached, so that it doesn't need to be repeatedly calculated. This special
# matrix has the attributes set and get (which set the matrix values and
# retrieve the matrix values) and setinverse and getinverse (which stores the
# inverse in the parent environment, and retrieves the inverse from the parent
# environment)

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


# cacheSolve checks to see if the inverse of a special CacheMatrix 
# has previously been computed. If it has, then it returns the 
# previous inverse; if it has not, then it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     ## Return a matrix that is the inverse of 'x'
     m
}
