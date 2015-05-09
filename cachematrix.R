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
	} else {
		message("cached data not found, calculating...")
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
		return(inv)
	}
}

## example usage:
## > m <- makeCacheMatrix()
## > m$set(matrix(1:4, 2, 2))
## > m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
## cached data not found, calculating...
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
