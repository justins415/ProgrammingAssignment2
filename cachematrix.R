## makeCacheMatrix produces a list containing four functions.
## In combination with cacheSolve, it calculates and/or stores the inverse of a matrix.
## Usage: matrix2 <- makeCacheMatrix(matrix1), then use cacheSolve(matrix2)
## "set" function stores the matrix, "get" retrieves it.
## "setinverse" stores the inverse; "getinverse" retrieves it.


makeCacheMatrix <- function(x = matrix()) {
     inv1 <- NULL
     set <- function(y) {
          x <<- y
          inv1 <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse1) inv1 <<- inverse1
     getinverse <- function() inv1
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes the list made by makeCacheMatrix and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv1 <- x$getinverse()
     if(!is.null(inv1)) {
          message("getting cached data")
          return(inv1)
     }
     data <- x$get()
     inv1 <- solve(data, ...)
     x$setinverse(inv1)
     inv1
}
