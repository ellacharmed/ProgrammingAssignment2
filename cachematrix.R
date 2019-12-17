## These 2 functions aim to teach us lexical scoping in R by leveraging the
## 2 functions in cachemean.R and using the built-in function solve to
## inverse a matrix and retrieve it back from cache if already existing

## use the 'solve' function to inverse a matrix 'x' passed as argument,
## also able to reset the matrix any number of times by using the set method

makeCacheMatrix <- function(x = matrix()) {
    theInverse <- NULL
    set <- function(anotherMatrix) {
        x <<- anotherMatrix
        theInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) theInverse <<- solve
    getinverse <- function() theInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## if inverse of matrix already present in cache, retrieve that
## otherwise use the 'solve' function to calculate the inverse 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    theInverse <- x$getinverse()
    if(!is.null(theInverse)) {
        message("getting cached data")
        return(theInverse)
    }
    data <- x$get()
    theInverse <- solve(data, ...)
    x$setinverse(theInverse)
    theInverse
}
