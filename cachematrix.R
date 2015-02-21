## Put comments here that give an overall description of what your
## functions do

## Function creates a special "matrix", which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      
    ## intiate values
    inverted <- NULL
      
  	## set the value of the matrix
  	set <- function(y = matrix()) {
	    x <<- y
	    inverted <<- NULL
  	}
      
  	## get the value of the matrix
  	get <- function() x

	## set the value of the inverse matrix
	setinvmatrix <- function(invMatrix) inverted <<- invMatrix

	## get the value of the inverse matrix
	getinvmatrix <- function() inverted
	list(set = set, get = get,
	   setinvmatrix = setinvmatrix,
	   getinvmatrix = getinvmatrix)
}


## function calculates the iverted version of the matrix created with the above function.
## it first checks to see if the iversion has already been calculated. If so, it `get`s the value from the
## cache and skips the computation.

cacheSolve <- function(x, ...) {
	## getting value from provided list
	inverted <- x$getinvmatrix()
	
	## check if we could bypass calculations
	if(!is.null(inverted)) {
	      message("getting cached data")
	      return(inverted)
	}

	## getting data for calculations
	data <- x$get()

	## calculating actual data
	inverted <- solve(data, ...)
	
	## puching result back to cache
	x$setinvmatrix(inverted)
	
	## return result
	inverted
}
