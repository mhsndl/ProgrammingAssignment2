## These functions cache the inverse of a matrix

## This function creates a special kind of a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	## Initialize the inverse with NULL
	inv <- NULL

	## Function to set the matrix
	set <- function(y) {
		matrix <<- y
		inv <<- NULL
	}

	## Function to get the matrix
	get <- function() {
		## return matrix
		matrix
	}

	## Function to set the inverse of a matrix
	setInverse <- function(inverse) {
		inv <<- inverse
	}

	## Function to get the inverse of a matrix
	getInverse <- function() {
		## return inverse
		inverse
	}

	## Return list of methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Get inverse of the matrix - x
	inv <- x$getInverse()

	## Return the inverse from the cache if inverse already calculated (i.e inverse is not NULL)
	if(!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	}

	## Calculate the inverse of a matrix if not calculated yet

	## Get the matrix
	data <- x$get()

	## Calculate the inverse of the matrix
	cinv <- solve(data) %*% data

	## Cache the inverse for future use
	x$setInverse(cinv)

	## Return inverse of the matrix
	cinv

}
