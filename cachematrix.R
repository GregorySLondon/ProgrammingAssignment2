## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# In this first function, we use a squared matrix/invertible
# for example, we could use x <- matrix(1:4,2,2)
# and create the special "matrix" to be cached in a parent environment
# to be retrieved

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL 
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        
        get <- function() x # Get the initial matrix
        setsolve <- function(solve) ix <<- solve # Invert the initial matrix with solve() and cache it to ix
        getsolve <- function() ix # Get the "ix" inverted matrix stored
        
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve) # List of the environment where the functions are cached
        
}


## Write a short comment describing this function
# This function is to test if the inverted matrix has already been calculated in a previous
# calculation. IF yes then it returns the cached otherwise it calculates the new inverted matrix
# and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ix <- x$getsolve() # Retrieve the cached inverted matrix
        if(!is.null(ix)) { # Test if ix is not Null (intial value) and that the inverted matrix has been cached
                
                message("Getting the inverse of the matrix cached")
                return(ix)
        }
        
        # If not the calculation of the inverted matrix is done and returned
        data <- x$get()
        ix <- solve(data, ...)
        x$setsolve(ix)
        return(ix) 
        
}
