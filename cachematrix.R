## These functions are able to cache potentially time-consuming matrix inverse computations.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean from 
## the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value 
## of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

## To run these functions:
## > my_inverseTest <- rbind(c(4,7),c(2,6))
## > my_inverseMatrix <- makeCacheMatrix(my_inverseTest)
## > cacheInverse(my_inverseMatrix)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4