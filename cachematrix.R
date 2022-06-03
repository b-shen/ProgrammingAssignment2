## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Initialization of object m
        m <- NULL
        ## Set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get the matrix
        get <- function() x
        ## Set the inverse of the matrix
        set_inv_matrix <- function(matryx) m <<- matryx
        ## Get the inverse of the matrix
        get_inv_matrix <- function() m
        ## Return a list()
        list(set = set, get = get, 
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}

## This function computes the inverse of the special "matrix" returned by 
## "makeCacheMatrix" above. If the inverse has already been calculated (and 
## the matrix has not changed), then "cacheSolve" should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inv_matrix()
        ## Returns the inverse matrix if the result is cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix 'x'
        data <- x$get()
        ## Computing the inverse of a square matrix using the solve function
        m <- solve(data)
        ## Set the inverse of the matrix to the object 'x'
        x$set_inv_matrix(m)
        ## Return the matrix
        m
}
