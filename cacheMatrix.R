## Caching inverse of a matrix
## Writing functions to store a matrix and later catch its inverse 

## Object to store the matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) { #setting the matrix
    x <<- y
    z <<- NULL
  }
  get <- function() x  #getting the matrix
  setinverse <- function(inverse) z <<- inverse #setting inverse of a matrix
  getinverse <- function() z  #getting inverse of a matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Catching the inverse of a matrix

cacheSolve <- function(x, ...) {
  z <- x$getinverse           
  if(!is.null(z)){          #Caching the data when z is not 0
    message("Getting cached data...")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...) #Otherwise, calculating the inverse by applying solve() to the data
  x$setinverse(z)
  z        ## Return a matrix that is the inverse of 'x'
}
