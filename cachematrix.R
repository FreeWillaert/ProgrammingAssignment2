## Matrix inversion can be a costly computation which may benefit from caching.
## The following is a set of R functions that enable caching of inverted matrices.
## Assumption: The matrix supplied is invertible.


# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# get/set the value of the matrix
# get/set the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getinverted <- function() inv
        setinverted <- function(invertedMatrix) inv <<- invertedMatrix
        list(
                get = get,
                set = set,
                getinverted = getinverted,
                setinverted = setinverted                
        )
}

# cacheSolve calculates the inverted matrix of the special "matrix" created with makeCacheMatrix. 
# It first checks if the inverted matrix has already been calculated. 
#  If so, it gets the inverted matrix from the cache and skips the computation. 
#  Otherwise, it calculates the inverted matrix of the data 
#  and sets the value of the inverted matrix in cache via the setinverted function.
cacheSolve <- function(x, ...) {
        inv <- x$getinverted()
        if(!is.null(inv)){
                print("Retrieved inverted matrix from cache.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverted(inv)
        inv
}