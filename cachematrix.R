## Pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inver <- NULL
  set <- function(y) {
    
    x <<- y
    inver <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This second function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}



matrix_created <- makeCacheMatrix(matrix(c(5,1,0,3,-1,2,4,0,-1),nrow=3))

matrix_created$get()

[,1] [,2] [,3]
[1,]    5    3    4
[2,]    1   -1    0
[3,]    0    2   -1


matrix_created$getInverse()

NULL


cacheSolve(matrix_created)

 cacheSolve(matrix_created)

[,1]    [,2]  [,3]
[1,] 0.0625  0.6875  0.25
[2,] 0.0625 -0.3125  0.25
[3,] 0.1250 -0.6250 -0.50



matrix_created$getInverse()

[,1]    [,2]  [,3]
[1,] 0.0625  0.6875  0.25
[2,] 0.0625 -0.3125  0.25
[3,] 0.1250 -0.6250 -0.50