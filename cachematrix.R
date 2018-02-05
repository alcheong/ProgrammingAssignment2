## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its 
## inverse.
## This function assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
	inv <<- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve # calculate the inverse
	getinv <- function() inv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. It first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinv function.

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if (!is.null(inv)) {
				message("getting cached data")
				return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
