## Matrix inversion can require large computational capacity
## thus it can be beneficial to cache the inverse of a 
## matrix to avoid repeated computation. The following
## two functions can be used to cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The function below enables the user to return the inverse of a matrix
## Initially the function checks to see if the inverse has already been
## computed, enabling the function to return the result without computation.
## If no inverse is found, the function computes the inverse.

## nb. This function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
