## For both functions, we assume that the matrix is square and invertible

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	## sets the matrix to y, and resets the inverse
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	## gets the stored matrix
	get <- function() x
	
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	
	list( set = set, get = get, 
			setinverse = setinverse,
			getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## The function uses the built-in solve() function to get the inverse.


cacheSolve <- function(x, ...) {
            i <- x$getinverse()	## grab cached value
            if(!is.null(i)) {	## if it's not null, return that value
                    message("getting cached data")
                    return(i)
            }
            data <- x$get()		## otherwise set data to x
            i <- solve(data, ...)	## and find inverse of data
            x$setinverse(i)		## and set that as the mean
            i
}
