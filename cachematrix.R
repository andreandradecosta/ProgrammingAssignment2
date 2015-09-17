##These two functions could be used to cache the result of inverting a matrix. Instead of calling solve each time, the result of the first call is saved in a cache. Example:
# Creating a matrix that cache its inverse:
# m <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))

# Getting it's inverse for the first time:
# cacheSolve(m)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

# Getting it's inverse for the second time:
# cacheSolve(m)
# getting cached data
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667


## makeCacheMatrix takes an ordinary matrix e returns a cacheable matix that it is able to save it's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve takes a cacheable matrix (the one returned by makeCacheMatrix) and returns it's inverse. For a given matrix, the inverse is calculated just once.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
