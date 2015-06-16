# 2 functions that hopefully compute and cache the inverse of a matrix

# Some key figures provided by Coursera to validate the 
# code would be very helpful. So I'm trying to setup my own test case and
# hope it will work

# create matrix
# ma = rbind (c(2,4), c(4,2))
# ma
# [,1] [,2]
# [1,]    2    4
# [2,]    4    2
# cma = makeCacheMatrix (ma)
# cma$get()
# [,1] [,2]
# [1,]    2    4
# [2,]    4    2

#### no message "Getting cached data ..." cause it's the first run

# cacheSolve(cma)
# [,1]       [,2]
# [1,] -0.1666667  0.3333333
# [2,]  0.3333333 -0.1666667

# cacheSolve(cma)
# Getting cached data ...
# [,1]       [,2]
# [1,] -0.1666667  0.3333333
# [2,]  0.3333333 -0.1666667

#### message "Getting cached data ..." cause it's the second run 


# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(ma = matrix()) {
        inv <- NULL
        set <- function(y) {
                ma <<- y
                inv <<- NULL
        }
        get <- function() ma
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}



# cacheSolve: 
# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache.

cacheSolve <- function(ma, ...) {
        inv <- ma$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data ...")
                return(inv)
        }
        data <- ma$get()
        inv <- solve(data)
        ma$setinverse(inv)
        inv
}