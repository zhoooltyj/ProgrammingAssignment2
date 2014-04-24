## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {    ## set x
                x <<- y
                m <<- NULL
        }
        
        get <- function() x     ## get x
        
        setinverse <- function(solve) m <<- solve       ## set m
        getinverse <- function() m                      ## get m
        
        ## Return a list with 4 items
        ## They are actually 4 functions wrapped in a list
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   
        

}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        m <- x$getinverse()     ## Query the x vector's cache
        
        if(!is.null(m)) {       ## If there is a cache
                message("getting cached data")
                return(m)       ## just return the cache, no computation needed
        }
        
        
        data <- x$get()         ## If there's no cache
        m <- solve(data, ...)   ## we actually compute them here
        x$setinverse(m)         ## save the result back to x's cache
        

        m                       ### Return a matrix that is the inverse of 'x'
        
}
