# Assignment 2 - Chandra Shekhar Kaushik
# This function creates the cache matrix. The return value from this function is to be passed
# cacheSolve function. 

# We can test the code using the below example
# x<-matrix(c(2,2,3,2),nrow=2,ncol=2)
# z<-makeCacheMatrix(x)
# cacheSolve(z)
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(z)
# getting cached data
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0

makeCacheMatrix<- function(x = matrix()) {
  # m_inv is the matrix inverse
  m_inv <- NULL
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m_inv <<- solve
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




# This function cacheSolve checks if there is already a cached version available
# If available will return the cached version else will calculate the inverse
# through the solve function and return the same. 
cacheSolve <- function(x, ...) {
  m_inv <- x$getinverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinverse(m_inv)
  m_inv
}
