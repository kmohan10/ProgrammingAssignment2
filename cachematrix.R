## Matrix inversion is a frequently used function that uses a lot of resources. The purpose
## of this function is to cache the Matrix inversion and use it when available.  Else the
## function calculates the matrix inverse.

## This is a special vector that creates a list containing a function to: 
## a. define the matrix
## b. get the matrix 
## c. calculate matrix inverse if necessary using solve function
## d. get matrix inverse

makeCacheMatrix <- function(x=matrix(1:9,3)) {
        y <- NULL
        set <- function(x) {
                
                solve(x)
                #Inverse matrix
                y <- solve(x)%%x
                #Store inverse in x
                x<<-y
                y <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) y <<- matrix
        getmatrix <- function() y
        list(set = set, get =get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the matrix inverse is cached. If yes, gets the matrix inverse.
## If no, calculates the matrix inverse and sets the value of the matrix in the cache.

cachesolve <- function(x, ...) {
        y <- x$getmatrix()
        if(!is.null(y)) {
                message("getting cached matrix")
                return(y)
        }
        data <- x$get()
        y <- matrix(data, ...)
        x$setmatrix(y)
        y
}
