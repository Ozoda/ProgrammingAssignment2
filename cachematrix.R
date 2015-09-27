## The below two functions 
## 1) create a special object that stores a square invertible matrix and
## 2) cache its inverse rather than computing it repeatedly 


## makeCacheMatrix function creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  v <- NULL
  set <- function(y){
    
    x <<- y
    v <<- NULL
  }
  
  get <- function()x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function()v
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## cacheSolve function  calculates the inverse of the matrix
## created in the makeCacheMatrix function. 
## It first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  
  my_matrix <- x$get()
  v <- solve(my_matrix, ...)
  x$setinverse(v)
  v
  
}
