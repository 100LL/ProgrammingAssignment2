## these functions store a matrix and its inverse so that 
## repeated calculation of the inverse can be avoided

## special "matrix" object that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse
	) 
}


## return the inverse of the matrix object,
## fetched from cache or calculated if not cached.
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
