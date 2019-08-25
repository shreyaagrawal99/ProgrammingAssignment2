

#Caches the matrix
makeCacheMatrix <- function(x=matrix()) {
  
  inver <- NULL
  
  set <- function(y) {
    x <<- y
    inver <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inver <<- inverse
  
  getinverse <- function() inver
  
  list(set=set, get=get, 
       setinverse = setinverse, 
       getinverse=getinverse)
  
  
}
  

#Checks if the inverse is in the cache and solves it if not
cacheSolve <- function(x, ... ) {
  
  inver <- x$getinverse()
  
  if(!is.null(inver)) {
    
    message("Getting from the cache!")
    return(inver)
  }
  
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver

}