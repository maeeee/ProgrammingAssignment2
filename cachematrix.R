## Caching the Inverse of a Matrix

## This function creats a "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL	
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## retrives the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInverse(inv)
        inv
}

## test 
## t_matrix <-makeCacheMatrix(matrix(c(2, 1, 5, 3), 2, 2))
## > t_matrix$get()
##      [,1] [,2]
## [1,]    2    5
## [2,]    1    3
## > cacheSolve(t_matrix)
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
## No cache when call the cacheSolve(t_matrix) first time
## > cacheSolve(t_matrix)
## getting cached data
##      [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
