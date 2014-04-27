## This function creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        # set the inverse value to NULL
        inverseMatrix <- NULL
        # to set the value of the matrix
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL 
        }
        # to get the value of the matrix
        get <- function() x
        # to set the inverse of the matrix
        setinverse <- function(inverse) inverseMatrix <<- inverse
        # to get the inverse of the matrix
        getinverse <- function() inverseMatrix
        # list the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Thisfunction calculates the inverse of the matrix created with the 
## above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets the value of the inverse in the cache via the
## the setinverse function.


cacheSolve <- function(x, ...) {
        # check if the inverse has already been cached
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        # if the inverse is not cached, get data from the matrix
        data <- x$get()
        # calculate the inverse of the matrix
        inverseMatrix <- solve(data, ...)
        # cache the inverse value
        x$setinverse(inverseMatrix)
        # print the results of the inverse of the matrix
        inverseMatrix
}
