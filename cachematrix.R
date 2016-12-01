# makeVector creates a special "vector", which is really a list containing a function to
# 
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

    slv <- NULL
    set <- function(y) {
        x <<- y
        slv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) slv <<- solve
    getsolve <- function() slv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# CacheSolve function calculates the inverse of the of the special "matrix" 
# created with the above function. 
# However, it first checks to see if the inverse of the matrix  has already been 
# calculated. If so, it gets the inverse of the matrix from the cache and skips
# the computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse of the matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    slv <- x$getsolve()
    if(!is.null(slv)) {
        message("getting cached data")
        return(slv)
    }
    data <- x$get()
    slv <- solve(data, ...)
    x$setsolve(slv)
    slv
}

my_Matrix <- matrix(c(1, 1, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
cacheSolve(makeCacheMatrix(my_Matrix))
solve(my_Matrix)
