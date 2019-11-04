## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCachematrix creates a special "matrix", which is a list containing a function to 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
  x <<- y
  z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## this function computes the inverse of the special matrix creatde by makeCacheMatrix function, 
## if inverse is already in cache then it will return it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting memory data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
  
}

##checking the results

M <- matrix(c(1,2,3,4),2,2)
M1 <- makeCacheMatrix(M)
cacheSolve(M1)
