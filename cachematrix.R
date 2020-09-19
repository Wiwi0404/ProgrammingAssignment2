## Create a special matrix

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) inv <<- invert
    getinvert <- function() inv
    
    list(set=set, get=get,
         setinvert=setinvert, 
         getinvert=getinvert)
}


## Calculate the invert of the special matrix created by the above function

cacheSolve <- function(x, ...) {
    
    inv <- x$getinvert()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvert(inv)
    inv
    
}
