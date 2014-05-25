## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                        ## create empty matrix
  set <- function(y) {
    x <<- y
    m <<- NULL                                      
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse    ## saves cache; takes matrix passed into it and stores it in m, the cache
  getinverse <- function() m                       ## returns cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- makeCacheMatrix(x)                         ## assign result of makeCacheMatrix(x) to variable "x"
  m <- x$getinverse                                  
  if(!is.null(m)) {                               ## if cache is empty
    message("getting cached data")                ## return message "getting cached data"
    return(m)                                     ## return cached data
  }
  data <- x$getinverse                                                                                                
  m <- solve(data, ...)                           ## find inverse matrix and assign to "m"
  x$setinverse(m)                                     
  m                                               ## return solved inverse matrix
}