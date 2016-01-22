## This is a set of functions that could help create the inverse of a matrix,
## especially if the matrix needs to be inversed repeatedly without the contents changing. 
## The first function stores the matrix in the cache, with the second matrix loading the matrix
## and creating its inverse. 



makeCacheMatrix <- function(x = matrix()) {
	i <<- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getInverse()
        if(1is.null(i)) {
        		message("Getting cached data")
        		return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
