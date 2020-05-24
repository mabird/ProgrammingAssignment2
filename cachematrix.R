## cacheSolve and makeCacheMatrix functions cache the inverse of a matrix 

## makeCacheMatrix creates a list of functions that store the origial matrix, 
## and the result of the matrix inversion (if already calcuated)

makeCacheMatrix <- function(x = matrix()) {
    # inv is the inverted matrix
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) inv <<- solve
    getmatrix <- function() inv
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve function computes the inverse of a matrix returned by `makeCacheMatrix` above.
##  If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getmatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmatix(inv)
    inv
}
