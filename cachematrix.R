## Functions for ProgrammingAssignment2:
## Framework for caching the computed inverse of a matrix so that it does
## not have to be computed again.

## makeCacheMatrix: creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        set_xinv <- function(solve) x_inv <<- solve
        get_xinv <- function() x_inv
        list(set = set,
             get = get,
             set_xinv = set_xinv,
             get_xinv = get_xinv)
}


## cacheSolve: computes the inverse of a matrix returned by makeCacheMatrix.
## If the inverse was already calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Cannot assume that the input matrix to solve is 
        ## square.  Have to do it ourselves.
        xrowcol <- dim(x)
        if( xrowcol[1] != xrowcol[2]) {
                print("Error: Input matrix to cacheSolve is not Square.")
                return
        }
        
        ## Here if x is square.  Compute its inverse, or retrieve it
        ## from cache
        x_inv <- x$get_xinv()
        if(!is.null(x_inv)) {
                message("Getting data from cache...")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data,...)
        x$set_xinv(x_inv)
        x_inv
        
}
