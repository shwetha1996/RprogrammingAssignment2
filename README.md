# RprogrammingAssignment2
second programming assignment will require you to write an R function is able to cache potentially time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. In this Programming Assignment will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.

## Assignment: Caching the Inverse of a Matrix
```
# Caching the Inverse of a Matrix #
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
```
