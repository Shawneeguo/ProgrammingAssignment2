## Put comments here that give an overall description of what your
## functions do:
## The following code includes two functions. The purpose of these two function is to cache the inverse of a matrix
## Write a short comment describing this function
## makeCacheMatrix:creats a special "matrix" object that can cache the inverse of the matrix.
## assume that the matrix supplied is always invertible
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           ##meaning no objects
     set <- function(y) { ##set the value of the matrix using another function
       x <<- y
       inv <<- NULL
     }
     get <- function() x ##get matrix x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}
## Write a short comment describing this function
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, the cacheSolve function should retrive the inverse from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {  ##check if the inverse is NULL
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...) ##using solve function to obtain the inverse
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
