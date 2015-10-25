# cachematrix is a special matrix that can cache its inverse
# basic getters and setters are provided

# creates a 'cachematrix' with the supplied matrix 'x'
# note: inverse is not initially calculated
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	set <- function(newMatrix) {
		inverse <<- NULL
		x <<- newMatrix
	}
	setinverse <- function(inv)
		inverse <<- inv

	get <- function()
		x
	getinverse <- function()
		inverse

	# this does something; for now it "works"
	list(set = set, setinverse = setinverse,
		 get = get, getinverse = getinverse)
}

# returns the cached inverse of a 'cachematrix'
cacheSolve <- function(x, ...) {
	# gets the inverse of matrix 'x'
	xInv <- x$getinverse()
	# checks if 'x$inverse' has been set
	if(is.null(xInv)) {
		# calculate the inverse of 'x' and cache
		xInv <- solve(x$get(), ...)
		x$setinverse(xInv)
	}
	# return the inverse of 'x'
	xInv
}
