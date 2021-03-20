##First create matrix using function and assign to makeCacheMatrix and set the matrix than ise get to get the matrix to inverse it

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {j <<- inverse}
  getInverse <- function() {j} 
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

##Now I will create function which will cache the inverse matrix and store in cacheSolve 

cacheSolve <- function(x, ...) {
  
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
