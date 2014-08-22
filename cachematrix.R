## These functions make it possible to store a matrix in a special object and
## cache the value of a matrix's inverse, which is an expensive computation 
## that should not be called repeatedly.

## Create an object that stores a matrix and a cached value of its inverse.
## The cached value must be set using the cacheSolve function defined below.
## WARNING! Do not call the setinverse function manually! It must be run in
## a controlled environment such as cacheSolve, which makes sure the inverse
## being passed in is the actual inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Given a CacheMatrix, created by the makeCacheMatrix function, calculate the 
## inverse of its stored matrix, and cache it using mackCacheMatrix$setinverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}


