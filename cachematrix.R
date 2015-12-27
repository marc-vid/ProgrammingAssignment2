## Description
## Functions to solve the inverse of a matrix
## or retrieve a cached pre inversed one
## Marcelo V. 2015-12-27


## create a object that can store and return a matrix
## and his inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the cached inverse
        inverseX <- NULL

        ## set a new matrix
        set <- function(y) {
                ## store the matrix
                x <<- y
                ## set to NULL the inverse of the new matrix
                inverseX <<- NULL
        ]
	
        ## return the stored matrix
        get <- function() x
	
        ## cache the inverse
        setinverse <- function(inverse) inverseX <<- inverse

        ## get the inverse
        getinverse <- function() inverseX

        ## create a list of the functions on this object
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## return the inverse of a matrix, if cached return the 
## cached version, if no, resolve and cache the inverse
## pre-assume that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## get the cached inverse
		ci <- x$getinverse()
		
		## verify if the inverse are cached
		if(!is.null(ci)) {
		        ## return the cached inverse
				return(ci)
		}
		
		## create and cache the inverse
		mtx <- x$get()
		ci <- solve(mtx)
		x$setinverse(ci)
		
		## Return a matrix that is the inverse of 'x'
        ci
}
