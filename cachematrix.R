## makeCacheMatrix: This function creates a matrix object that stores a matrix and caches its inverse.
## It also provides functions to access data and the inverse as follows:
##   get(): Returns the current matrix.
##   set(): Sets the matrix value.
##   getinverse(): Returns the inverse of the matrix. If the cached value is available, the cached data is returned
##      else it returns NULL.
##
## These functions are returned in the form of a list.
## NOTE: The caching is implemented by leveraging the lexical scoping capabilities of R: The "cache" is essentially
## created by defining a variable within the scope of a function, thus segregating it from the global
## environment. Updates are performed using the <<- operator that allows you to access variables defined
## in the parent context.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function returns the inverse of the special matrix object created via the makeCacheMatrix
## function. If the object has its inverse cached, the cached value is returned; if not, then the inverse is
## calculated and set in the matrix object and returned as the output.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
