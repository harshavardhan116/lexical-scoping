## Put comments here that give an overall description of what your
## functions do
##Caching the inverse of a matrix.

## Write a short comment describing this function
##This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##cacheSolve is a function wich computes the inverse of the special matrix
##makeCacheMatrix above, if the inverse has already been calculated
##(matrix not changed) then it should retrieve the inverse of the cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached result")
                return(inv)
        }

        mat <- x$get()
        inv <-solve(mat, ...)
        x$setInverse(inv)
        inv
}
