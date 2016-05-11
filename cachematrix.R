## Put comments here that give an overall description of what your
## functions do

# The following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# The following function calculate the inverse of the matrix created by 'makeCacheMatrix' above
# First it checks whether the inverse is already calculated, if so it gets from the cache
# Otherwise, it will calculated the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
