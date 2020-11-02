
## 20201101 for Coursera-R week 3 programming assignment

###############
### CACHING ###
###############

# Matrix inversion are potentially time-consuming computations.
# Caching computed results for future use rather than repeated recomputation saves time.
# This function creates a special "matrix" object that can cache the inverse of a matrix.

## makeCacheMatrix based on the makeVector function:
# 1. set input x as a matrix  
# 2. set p as NULL
# 3. replace "mean" with "solve"

makeCacheMatrix <- function(x = matrix()) {
        p <- NULL
        set <- function(y) {
                x <<- y
                p <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) p <<- inverse
        getInverse <- function() p
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##################
### RETRIEVING ###
##################

## The cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 

# If the inverse has already been calculated with the matrix unchanged, 
# then this cacheSolve function should retrieve the inverse from the cache.

## cacheSolve based on the cachemean function
# 1. replace m with p; data with g
# 2. replace "mean" with "solve"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        p <- x$getInverse()
        if (!is.null(p)) {
                message("getting cached data")
                return(p)
        }
        g <- x$get()
        p <- solve(g, ...)
        x$setInverse(p)
        p
}








