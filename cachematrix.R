## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse martix
    inv <- NULL
    # set the value of matrix
    set <- function( matrix ) {
        x <<- matrix
        inv <<- NULL
    }
    
    # get the original matrix
    get <- function() {
        # Return x
        x
    }
    
    # set inverse of given matrix 
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # get the inverse of the matrix
    getInverse <- function() {
        # return inv
        inv
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## return the inverse if its already cached
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix x
    matrix_x <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(matrix_x) %*% matrix_x
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}

