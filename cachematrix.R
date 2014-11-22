# Description for makeCacheMatrix

#First we set the value of the matrix, then get the value of the matrix
#Then then we set the inverse and then get the value of the inverse
#inv variable contains the cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x

setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function evaluates if the inverse is already in cache, 
#then it fetches it and returns the inverse , otherwise it computes the inverse and returns it
cacheSolve <- function(x, ...) {
inv <- x$getinv()
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv
}
