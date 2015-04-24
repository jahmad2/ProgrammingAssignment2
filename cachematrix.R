makeCacheMatrix <- function(x = matrix()) {
  #creating function
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #setting and returning function
  get <- function() x
  setinverse <- function(solved) m <<- solved
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  #get cached inverse
  m <- x$getinverse()
  #solve if not null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}