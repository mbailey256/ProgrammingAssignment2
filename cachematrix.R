## The following functions store a matrix and the results of the 
## inverse in cache


## The first function, makeCacheMatrix creates an object that stores 
## the matrix and the cache of it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function tests to see if the inverse of the matrix stored within 
## the object created by makeCacheMatrix has been calculated and returns the 
## stored value, if not it stores the inverse of the matrix using the solve 
## function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m     
        
}
