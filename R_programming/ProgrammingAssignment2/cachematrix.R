## The following functions are used to create a special "CacheMatrix" object 
## that allows to store in cache the value of the inverse of the matrix.
## In this way if one wants to calculate the inverse of the "CacheMatrix" this
## value will be retrieved from the cache if it was already calculated or it
## will calculate the inverse and store it in the cache for future acces.


## This function takes as an argument a matrix and creates a new object 
## "CacheMatrix". The new object is a list of functions that can be applied
## to the new object "CacheMatrix"

makeCacheMatrix <- function(x = matrix()){
    inverse <- NULL			# Set the cache value of inverse to NULL
    set <- function(y){		# Set the value of the matrix using super-assignment
        x <<- y				
        inverse <<- NULL
    }
    get <- function() x	    # Retrieves the matrix
    setinverse <- function(solve) inverse <<- solve # Sets the inverse in cache
    getinverse <- function() inverse # Retrieves the inverse from cache
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the inverse from a "CacheMatrix" object.
## If the inverse its being calculated for the first time it will store it
## in the cache. Otherwise it reads it from the cache.

cacheSolve <- function(x, ...){
    inverse <- x$getinverse()	# Reads the cache of the CacheMatrix
    if(!is.null(inverse)) {		# If the values does not exist it calculates it
        message("getting inverse from cached data")
        return(inverse)
    }
    data <- x$get()				# If the inverse exist in cache it retrieves it
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
