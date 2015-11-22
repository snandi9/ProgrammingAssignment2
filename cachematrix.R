##The makeCacheMatrix function takes a 2X2 input matrix and displays the inverse values based on the inverse ##calculation from cacheSolve function based on the condition for cached data. "x <<- y" substitutes the vector ##x with y (the input) in the main ##function (makeCacheMatrix)."m <<- NULL" restores to null the value of the ##solve m. 

## This function stores the value of the input in a variable m into the main function makeCacheMatrix 
##(setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix(c(1,2,3,4),nrow=2,ncol=2)) {
	    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## CacheSolve does the calculation for the inverse of the matrix and the input of cacheSolve is the object where ##makeCacheMatrix is stored.cacheSolve verifies the value m, stored previously with getinverse, exists and is ##not NULL. If it exists in memory, it simply returns a message and the value m

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
