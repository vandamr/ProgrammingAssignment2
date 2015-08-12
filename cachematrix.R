## Data Scientist - R Programming W3 - assignment 2

## Create a list of 4 matrix functions for matrix x
makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  setmatrix <- function(y) {                   ## $setmatrix Sets the data matrix in cache
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x                    ## $getmatrix gets the data matrix from cache
  setinverse <- function(solve) inv <<- solve  ## $setinverse sets the inverse matrix in cache
  getinverse <- function() inv                 ## $getinverse gets the inverse matrix from cache
  list(setmatrix=setmatrix,                    ## return the list of 4 functions
       getmatrix=getmatrix,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Calculates the inverse of the matrix x. If the inverse
## is already calculated, it will not calculate but take it from cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                        ## get the inverse matrix from cache
  if(!is.null(inv)) {                          ## determine if the inverse is already calculated
    message("getting inverse from cache")      ## print the message to the console
    return(inv)                                ## return the inverse matrix
  }
  matrix <- x$getmatrix()                      ## get the data matrix from cache
  inv <- solve(matrix)                         ## calculate the inverse matrix
  x$setinverse(inv)                            ## set the inverse matrix in cache
  inv                                          ## return the inverse matrix
}