## Put comments here that give an overall description of what your
## functions do
## These functions enable us to cache and retrieve the result of a matrix inverse operation 
## in order to avoid duplicated calculations of matrix inverse during a loop
## Write a short comment describing this function
## This function provides a means of caching a matrix together with it's inverse (using 'set' functions)
## and provides the means to access both (through 'get' functions)
makeCacheMatrix <- function(x = matrix()) {
	z <- matrix()
	set <- function(y){
		x <<- y
		z <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) z <<- inverse
	getinverse <- function() z
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}
## Write a short comment describing this function
## This function accesses the inverse of a matrix if has already been calculated for that matrix and cached
## or if not then it calculates the inverse and then stores it to cache using the setinverse function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	z <- x$getinverse()
	if(!is.null(z)) {
		message("getting cached inverse matrix")
		return(z)
	}
	sqmatrix <- x$get()
	z <- solve(sqmatrix)
	x$setinverse(z)
	z
}
