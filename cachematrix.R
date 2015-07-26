## To improve performance it makes sense to cache results of resource-intensive 
## calculations. 
##
## The function 'makeCacheMatrix' returns an R object that stores original and cached
## values of matrix type and provides getter/setter methods. It's not specific to
## calculation of inverse matrix and can be used to cache any matrix values. 
##
## The second function 'cacheSolve' calculates the matrix inversion trying to
## get value from the cache first. If there is no cached result yet it uses
## the 'solve' function of the base R package to calculate the result.



# Returns an object that stores original and cached matrix with getter/setter methods.
# Parameter x - matrix that will be used in resource-intensive calculations. 
makeCacheMatrix <- function(x = matrix()) {
	cachedResult <- NULL
	set <- function(y) {
		x <<- y
		cachedResult <<- NULL 	# clear cached value	
	}
	get <- function() x
	setCache <- function(newValue) cachedResult <<- newValue
	getCache <- function() cachedResult 

	# return a list of four functions specified above
	list( set = set, 
		get = get,
		setCache  = setCache,
		getCache = getCache)
}


# Returns a matrix that is the inverse of 'x'. Uses cached result if possible.
# Parameter x - 'matrix' cache object returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
      ## First try to get result from the cache
	result <- x$getCache() 

	if (!is.null(result)) {
		message("Found data in cache")
		return(result)
	}

	## There is no data in the cache yet

	## Calculate inverse matrix
	matrixToSolve <- x$get()
	result <- solve(matrixToSolve, ...)

	## Store result in the cache 
	x$setCache(result)

	return(result)	
}