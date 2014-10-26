## This implements a matrix that can cache its inverse, which avoid duplicate computation.

## Create a clean matrix with no data and null inverse value.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        
        ## setter and getter of matrix.
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## getter and setter of the inversed matrix.
        setinverse <- function(i) m <<- i
        getinverse <- function() m
        
        ## Initialize a list with all above methods and properties.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Get the inverse value of a matrix from cache. 
## If the value is null, calculate the inverse and store it into cache.
cacheSolve <- function(x,...){
        m <- x$getinverse()
        
        ## if m has a value, return it directly.
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## The m is null, so we compute it out of matrix and set it to cache.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}
