## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) xinv <<- inv
    getInverse <- function() xinv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(FunVec, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- FunVec$getInverse()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    x <- FunVec$get()
    xinv <- solve(x)
    FunVec$setInverse(xinv)
    xinv
}
