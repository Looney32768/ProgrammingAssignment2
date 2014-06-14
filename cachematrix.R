## Put comments here that give an overall description of what your
## functions do

## Makes a wrapper for an invertible matrix 
## that is also keeping its inverse for later re-use
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL     # holds inverse of matrix x
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


## Returns the inverse of x whicj is a matrix wrapped with makeCacheMatrix
## may skip re-computation of inverse if the underlying matrix hasn't changed between the calls
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}

##
## Tests
##
testAdequacy <- function() {
    m <- matrix(rnorm(100), 10, 10)
    cm <- makeCacheMatrix(m)
    t <- c(solve(m) == cacheSolve(cm), solve(m) == cacheSolve(cm))
    r <- sum(t) == length(t)
    message("cacheSolve(cm) returns correct values both times: ", r)
    r
}

testCacheSolve <- function() {
    cm <- makeCacheMatrix(matrix(rnorm(1000000), 1000, 1000))
    time1 <- system.time(cacheSolve(cm))['elapsed']
    time2 <- system.time(cacheSolve(cm))['elapsed']
    r <- time1 > time2 * 10
    message("call#1 took significantly longer than call#2: ", r)
    r
}

testRecomputeInverseOnMatrixChange <- function() {
    cm <- makeCacheMatrix(matrix(rnorm(100), 10, 10))
    cacheSolve(cm)
    m2 <- matrix(rnorm(100), 10, 10)
    cm$set(m2)
    t <- solve(m2) == cacheSolve(cm)
    r <- sum(t) == length(t)
    message("Matrix inverse has been re-computed after underlying object changed: ", r)
    r
}

runTests <- function() {
    r <- testAdequacy() && testCacheSolve() && testRecomputeInverseOnMatrixChange()
    message("All tests passed OK: ", r)
    r
}