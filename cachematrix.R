
# The idea is to be able to cache potentially time-consuming computations.
# These functions create in R something similar to Objects in other languages
# Thus, this objects are able to mantain state and cache the mentioned computations


## returns a list with setter an getter functions for matrix and matrixInverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    #seting a new matrix should also clear it's inverse
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) { i <<- inverse }
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## computes the inverse of matrix, if it has not been cached already
## expects, as a parameter, an instance of makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

# makes a simple test of the previous functions 
testCache <- function() {
    mkm <- makeCacheMatrix()
    mkm$set(matrix(3:6,2)) #sets the initialvalue
    message('first use')
    print(cacheSolve(mkm)) #first use, computes the matrix
    message('second use')
    print(cacheSolve(mkm)) #second use, uses the cached inverse
}
