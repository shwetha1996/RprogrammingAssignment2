makeCacheMatrix <- function(x = matrix()) {
  j <- NULL             ##  inverse matrix as null
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x                             # get the value of the Matrix
  setInverse <- function(inverse) j <<- inverse        #setting the value of the invertible matrix
  getInverse <- function() j                              #getting the value of the invertible matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){                                     #if inverse matrix is not NULL
    message("getting cached data")                    #Type of the messeage: Getting a Cached Invertible Matrix
    return(j)                                            #returning to the invertible matrix
  }
  mat <- x$get()                          #get the the original Matrix Data
  j <- solve(mat,...)
  x$setInverse(j)                             # setting the invertible matrix
  j                                           ##returning to the invertible matrix
}