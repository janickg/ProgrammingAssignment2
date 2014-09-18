## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x=numeric()) 
{
    # initialize local member variable within this function
    inverseMatrix <- NULL
    
    
    # sets matrix object with input argument and
    # initializes inverseMatrix in the enclosing frame
    set <- function(y)
    {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    
    # returns the matrix object
    get <- function()
    {
        return(x)
    }
    
    
    # stores the inverse matrix into m
    setinverse <- function(setMatrix)
    {
        # check to see if setMatrix is a square matrix
        if (nrow(setMatrix) != ncol(setMatrix))
        {
            stop("you are trying to set a non-square matrix as
                  the inverse matrix")
        }
    
        # set to inverseMatrix in the enclosing frame
        inverseMatrix <<- setMatrix
    }
    
    
    # return the inverse matrix
    getinverse <- function()
    {
        return(inverseMatrix)
    }
    
    
    # list containing a reference to each function inside makeCacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}



## This function computes the inverse of the special "matrix", x, returned
## by makeCacheMatrix. If the inverse of the special matrix has already
## been calculated, then return the inverse from the cache.
cacheSolve <- function(x, ...)
{
    # invoke the getinverse function on the special object matrix
    m <- x$getinverse()
    
    # if m is not null, there exists a cached copy of the inverse matrix
    # return the inverse matrix from the cache
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    ## if m is null, then we need to proceed and calculate the inverse of m
    
    # get the matrix
    data <- x$get()
    
    # invoke sole to calculate the inverse and assign to m
    m <- solve(data, ...)
    
    # set the inverse of matrix to the cache
    x$setinverse(m)
    
    # return the inverse
    return(m)
}
