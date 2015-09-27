## This code contains a pair of functions that compute and cache the inverse of a matrix.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	newmatrix <- function(new) {
		x <<- new		## assign new matrix to x 
		m <<- NULL		## set inverse matrix to null if a new matrix is made
	}

	getmatrix <- function() x		## return matrix x

	setinverse <- function(matrix) m <<- matrix		## cache the inverse calculated

	getinverse <- function() m		## get the cached inverse
	
	list (newmatrix = newmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	m <- x$getinverse()

	if(!is.null(m)){
		message("getting data from cache...")
		return(m)			## if m is not empty then return m
	}
	
	matrix <- x$getmatrix()		## if m is empty, then getmatrix created in makeCacheMatrix function's output list
	
	m <- solve(matrix)		## compute the inverse of matrix
	
	x$setinverse(m)			## set the inverse in makeCacheMatrix output list
	
	m					## return the previously computed inverse
      
}
