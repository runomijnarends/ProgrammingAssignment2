## Function makeCachedMatrix creates a cached version of an existing matrix variable x.
## Function cacheSolve returns the inverse of a matrix by evaluating the solve() function
## or, if the inverse has already been stored in the cache, by retrieving the result from
## the cache.

## This function creates a matrix variable along with a cached matrix variable that is used to
## store the result of calculations performed on the matrix variable. It returns a
## list of functions that can be used by other functions to set or access this cache. This
## approach can save computational time when repeatedly evaluating the same particular
## computationally expensive calculations on the matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes a matrix as an argument and checks if the inverse
## of this matrix has already been calculated and stored in the cache previously.
## If so, the function retrieves the calculated value of the inverse from the cache and
## returns it; If not, the function calculates the inverse of the matrix (using solve())
## and returns this inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m 
}

