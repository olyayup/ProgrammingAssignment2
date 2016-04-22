
## This is a programming assignment 2
## two functions are created: the first one to enable caching of the inverse matrix
## and the second to retrieve the cached inverse matrix (if it is calculated) or calculate (if cached one is NULL) 


## makeCacheMatrix creates a a special "matrix" capable to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invm <<- solve
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This functions checks whether the inverse matrix has been already calculated and returns either cached inverse matrix or 
## calculates it

cacheSolve <- function(x, ...) {
	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
	m
}
