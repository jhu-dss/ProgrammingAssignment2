## rather than calculate the inverse of (the same) matrix repeatedly,
## a cache is created and checked before calculating


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	Inverse <- NULL
	
	Set <- function(y) {
		x <<- y
		Inverse <<- NULL
	}
	Get <- function() x
	SetInverse <- function(x) Inverse <<- x
	GetInverse <- function() Inverse
	list(Set = Set, Get = Get, SetInverse = SetInverse, GetInverse = GetInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then then cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	Inverse <- x$GetInverse()

	if (!is.null(Inverse)){
		message("getting cached data")
		return(Inverse)
	}

	SourceMatrix = x$Get()
	Inverse <- solve(SourceMatrix, ...)

	x$SetInverse(Inverse)
	return(Inverse)
}