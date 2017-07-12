## The functions below are meant to return the inverse of a matrix
## One particularity is to return the data from the cache when it has already
## been calculated or calculates it and stores it in cache when not

## Function makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.
## which is really a list containing a function to :
## set: set the value of the matrix
## get: get the value of the matrix
## setsolve: set the value of the inverse matrix
## getsolve: get the value of the inverse matrix
## 
## Example:
## x <- matrix( c(4, 2, 2, 2, 3, 1, 2, 1, 3), nrow=3, byrow=TRUE)
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
        # Intiate the inverse matrix to NULL
        inv <- NULL
        
        # Store the matrix to be inversed in cache and initiate the inverse
        # matrix to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Return the matrix to be inversed
        get <- function() x
        
        # Store the inverse matrix in cache
        # (manual storing without computation)
        setsolve <- function(solve) inv <<- solve
        
        # Return the inverse matrix currently stored in cache
        getsolve <- function() inv
        
        # Encapsulate the 4 functions above into a list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Function cacheSolve:
## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates
## the inverse matrix of the data and sets the value in the cache via the
## setsolve function.
## 
## ## Here, we'll always consider the matrix to be invertible.
## ## If not, prior to the computation, we could perform the following checks:
## ## - is the matrix square or not?
## ## - is the matrix singular or not?
## 
## Example:
## x <- matrix( c(4, 2, 2, 2, 3, 1, 2, 1, 3), nrow=3, byrow=TRUE)
## m <- makeCacheMatrix(x)
## cacheSolve(m)
##       [,1]  [,2]  [,3]
## [1,]  0.50 -0.25 -0.25
## [2,] -0.25  0.50  0.00
## [3,] -0.25  0.00  0.50
## cacheSolve(m)
## Getting inverse matrix from cache
##       [,1]  [,2]  [,3]
## [1,]  0.50 -0.25 -0.25
## [2,] -0.25  0.50  0.00
## [3,] -0.25  0.00  0.50

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Get the current state of the inverse matrix in cache
        inv <- x$getsolve()
        
        # Test if the inverse matrix has been computed yet
        if(!is.null(inv)) {
                # If yes, the function returns the inverse matrix from cache
                message("Getting inverse matrix from cache")
                return(inv)
        }
        
        # If not (the matrix hasn't been calculated yet), get the matrix
        # to be inversed from cache into 'data'
        data <- x$get()
        
        ## In our example, the matrix is supposed to be invertible. If not,
        ## we could perform the following tests:
        
        ## if(nrow(data) != ncol(data)) {
        ##         message("The stored matrix is not square and has no inverse!")
        ##         ## Test if data is not a square matrix
        ## } else if(det(data) == 0) {
        ##         ## Test if data is a singular matrix
        ##         message("The stored matrix is singular and has no inverse!")
        ## } else {
        
        # Compute the inverse of the matrix retrieved from cache
        inv <- solve(data, ...)
        
        # The result is frist stored into the cache
        x$setsolve(inv)
        
        # And then, the result is returned
        inv
        
        ## }
}
