## Caching the inverse of a matrix

## makeCacheMatrix creates list containing a function to
## 1. set value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns inverse of the matrix.  It first checks if the inverse has already been computed.  If so, it gets the result.  If not, it computes the inverse, sets the value in the cache by the setinverse function.

## assumes that function is invertible

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
