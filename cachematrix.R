# Programming Assignment 2: Lexical ScopingProgramming

# This Programming Assignment take advantage of the scoping rules of the R language 
# and how they can be manipulated to preserve state inside of an R object.

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

# Usage
# > x <- matrix(rnorm(25), nrow = 5)
# > cx <- makeCacheMatrix(x)
# > cx$get()
#             [,1]        [,2]       [,3]      [,4]        [,5]
# [1,] -0.41811797  0.11219882  1.1785876 0.4964133 -0.33160531
# [2,] -0.51913554  0.02445704 -0.2739018 0.2629996  0.04353292
# [3,] -0.01344628 -0.10477473 -1.6937074 0.2094907  0.87096808
# [4,]  2.43460970  1.42308202 -0.2804437 1.0354776  1.10066515
# [5,]  1.78897521 -0.50140719 -0.2867011 1.0752731 -0.67043093

# > cacheSolve(cx)
#             [,1]       [,2]        [,3]        [,4]       [,5]
# [1,] -0.09682675 -0.8678890  0.03025541  0.08455413  0.1696577
# [2,] -1.59608075  3.1269277 -1.66806126  0.56495214 -0.2470230
# [3,]  1.11942360 -1.6949644  0.49650583 -0.06919570 -0.1323240
# [4,]  0.82778598  0.5401934  0.43241555  0.05531068  0.2782041
# [5,]  1.78425794 -3.0632420  1.80946325 -0.07859610 -0.3513330
# > 
