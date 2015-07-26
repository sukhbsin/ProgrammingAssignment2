## Put comments here that give an overall description of what your
## functions do
# These functions allow us to cache the inverse of an invertible matrix. The input matrix is assumed to be invertible to begin with. No checks are performed on the invertability of the matrix. 

#AUTHOR: SUKHBIR SINGH, 26-JUL-2014

## Write a short comment describing this function
# makeCacheMatrix takes a vanilla matrix object and creates a 'rich' matrix object from it. This 'rich' object exposes s# several getter/setter methods, which are used to store the value of the inverse of the matrix in a separate environment

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    
   	set <- function(y) {
        	x <<- y
        	inverse <<- NULL
	}
    
	get <- function() x
    
    	setInverse <- function(inverse_matrix) inverse <<- inverse_matrix
    
    	getInverse <- function() inverse
    
    	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve function works on the 'rich' matrix object created by the makeCacheMatrix function, and uses the latter's getter/setter functions to cache the inverse of the matrix.

cacheSolve <- function(x, ...) {
    	## Return a matrix that is the inverse of 'x'
    	
	inverse_matrix <- x$getInverse()
    	if(!is.null(inverse_matrix)){
        	message("Getting cached inverse")
        	return(inverse_matrix)
    	}
    
    	data <- x$get()
    	inverse_matrix <- solve(data, ...)
    
    	x$setInverse(inverse_matrix)
    
    	inverse_matrix
    
}
