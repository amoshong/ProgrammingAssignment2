## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that can cache its inverse.
## This function creates a cache set up for a given matrix x. It returns 4 functions which can be called
## 1. set: replaces the starting matrix and sets the cached xinv to NULL
## 2. get: returns the matrix x
## 3. setInverse: sets the inverse
## 4. getInverse: returns the inverse
  
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse and stores the result in the given cache object via 
## the setInverse function then returns Xinv

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
