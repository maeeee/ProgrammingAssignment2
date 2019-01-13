## Caching the Inverse of a Matrix

## This function creats a "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL  ## initialize the matrix inverse as NULL
	set <- function(y) {
		x <<- y  ## set the matrix globally
		inv <<- NULL	
	}
	get <- function() x ## get the matrix x
	setInverse <- function(inverse) inv <<- inverse  ## set the inversion
	getInverse <- function() inv   ## get the inversion
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## retrives the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inversion of 'x'
        inv <- x$getInverse() ##get the inversion of x and assign it to inv
        ## if !is.null(inv) is TRUE, this means we already calculated the inversed matrix
        if(!is.null(inv)) {
        	message("getting cached data")  ## pring the massage "getting cached data"
        	return(inv) ## return the cached inv
        }
        ## if no cached data was found, do the following
        matr <- x$get() ## get the matrix and assign it to matr
        inv <- solve(matr, ...) ## calculate the inversion using function solve()
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
