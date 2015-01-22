## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
## It creates a list containing a function to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() retuns a matrix that is the inverse of 'x'.
## However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the data and sets the
## value of the mean in the cache via the setInverse function.

## Assumption: the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}


## TEST CASE
## > x <- rbind ( c(1,2), c(3,4))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## To show that no cache is present in first run
## > cacheSolve(m)
##       [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## To show retrieving of cached value in second run
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5