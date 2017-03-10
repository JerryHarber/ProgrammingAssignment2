
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## Input is a numeric square matrix, x
	## The matrix will be cached in another environment
	## A list of functions will be returned
	## that will operate on the cached matrix, i.e., solve(x)
	
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Returns a matrix that is the inverse of the x matrix that was used in the makeCacheMatrix
	## The input x here is a list functions that can operate on the cached matrix
	
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}