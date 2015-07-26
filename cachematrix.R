## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	cachedResult <- NULL
	set <- function(y) {
		x <<- y
		cachedResult <<- NULL 	# clear cached value	
	}
	get <- function() x
	setCache <- function(newValue) cachedResult <<- newValue
	getCache <- function() cachedResult 

	# return list of four functions specified above
	list( set = set, 
		get = get,
		setCache  = setCache,
		getCache = getCache)
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      ## First try to get result from the cache
	result <- x$getCache() 

	if (!is.null(result)) {
		message("Found data in cache")
		return(result)
	}

	## There is no data in cache yet

	## Calculate inverse matrix
	matrixToSolve <- x$get()
	result <- solve(matrixToSolve, ...)

	## Store result in the cache 
	x$setCache(result)

	return(result)	
}
