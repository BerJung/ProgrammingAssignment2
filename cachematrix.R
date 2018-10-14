## The code written below is created to figure out 
## how to get some benefit benefit to caching 
##the inverse of a matrix rather than computing it repeatedly. 


## "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) a <<- inverse
    getInverse <- function() a
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## "cacheSolve" is a function that compute the inverse of 
## the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    a <- x$getInverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setInverse(a)
    a
        ## Return a matrix that is the inverse of 'x'
}
