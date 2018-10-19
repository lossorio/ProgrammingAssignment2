## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function
##
## Edits made to script by Luis Ossorio 10/18/2018
##
## This function creates a special Matrix object that can cache its inverse
## that can save compute time if needed more than once

makeCacheMatrix <- function(x = matrix()) {     ## argument has default more of "matrix"
  inv <- NULL                                   ## place to hold value of inverse
  set <- function(y) {                          ## define set function to set new 
    x <<- y                                     ## value of matrix in global environment
    inv <<- NULL                                ## clear place holder
  }
  get <- function() x                           ## define get function value of matrix returned 
  
  setinverse <- function(inverse) inv <<- inverse  ## value of inv in global environment
  getinverse <- function() inv                     ## gets value of inv 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #  
  
}


## Write a short comment describing this function
#
## This function computes the inverse of special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated and matrix has not changed, 
## cacheSolve will pull inverse matrix from cache and save compute time

cacheSolve <- function(x, ...) {
  # get value of the matrix from prior makeCacheMatrix function
  inv <- x$getinverse()           
  if (!is.null(inv) ) {             # if inverse matrix is not NULL
    message("getting cached data")  # message user we are using cached value
    return(inv)                     # return the cached matrix
  }
  data <- x$get()                   # if we are here we need to calculate so get matrix data
  inv <- solve(data, ...)           # call solve function to get inverse matrix
  x$setinverse(inv)                 # set value to save 
  inv
        ## Return a matrix that is the inverse of 'x'
}

# Testing above functions
#
#> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#> my_matrix$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#
#
#> my_matrix$getinverse()
#NULL
#
#
#> cacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
