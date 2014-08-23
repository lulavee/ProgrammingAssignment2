## Put comments here that give an overall description of what your
## functions do

## This function creates a internal cache to storage the inverse matrix of x
makeCacheMatrix <- function(x = matrix()) {
  
        # I don`t know the inverse new value 
        inv <- NULL
        
        # "set" is a function to change the internal value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
  
        # "get" is a function that return x 
        get <- function() x
  
        # "setinv" is a function that cache inverse value
        setinv <- function(inverseMatrix){
                        inv <<- inverseMatrix
                }
  
        # "getinv" is a function that return the cache inverse value
        getinv <- function() inv
  
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## This function calculates the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # recovering the cache value if is there
        inv <- x$getinv()
        
        # Is not the recoverd value NULL?
        if(!is.null(inv)) { 
                message("getting cached data")
                #then I don't need to compute it
                return(inv)
        }
        
        #If is NULL, then I should caculate it
        #get the data from x
        data <- x$get()
        value <- solve(data, ...)
        x$setinv(value)
        value      
}
