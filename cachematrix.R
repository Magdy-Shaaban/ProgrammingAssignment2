## assignment for lexical scoping


## creates a special "vector", which is really a list containing a function to
#1.	set the matrix
#2.	get the matrix
#3.	set the inverse of the matrix
#4.	get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) v <<- solve
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


##The following function calculates the inverse of the special "matrix" created 
#with the above function. However, it first checks to see if the inverse has 
#already been calculated. If so, it gets the inverse from the cache and skips 
#the computation. Otherwise, it calculates the inverse of the matrix and sets the
#matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v  
  
  
  
}


