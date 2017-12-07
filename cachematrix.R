## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) { ## set the value of the matrix
		x <<- y
		i <<- NULL
	}
	get <- function() x ## get the value of the matrix
	
	setinverse <- function(solve) i <<- solve
	## set the value of the inverse
	getinverse <- function() i
	## get the value of the inverse
	
	matrix(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
	
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)){
        	message ("getting cached data")
        	return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
