## The makeCacheMatrix function creates a cache matrix for storing data
## for later retrieval.  Functions included within the makeCacheMatrix function are:
## set(y) - Sets the value of the matrix to 'y' and initializes the inverse to NULL
## get() - gets the value of the matrix
## setinverse(inverse) - sets the value of the inverse to 'inverse'
## getinverse() - gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ##set the value of the matrix
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    #get the value of the matrix
    get <- function()
    {
        x
    }
    
    #set the value of the inverse
    setinverse <- function(inverse)
    {
        inv <<- inverse
    }
    
    #get the value of the inverse
    getinverse <- function()
    {
        inv
    }
    
    #create a list of the functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## The cacheSolve function calculates the inverse of a cached matrix and returns
## the value

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    
    ## if the inverse has already been calculated, return the inverse
    if(!is.null(inv))
    {
        message("getting chached data")
        return(inv)
    }
    
    ##otherwise, get the matrix value, calculate the inverse and return its value
    data <- x$get()
    inv <- solve(data) 
    x$setinverse(inv)
    inv
    
}
