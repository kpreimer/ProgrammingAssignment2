## The two functions in this script are intended to illustrate how scoping rules  
## in R andhow they can be manipulated to preserve state inside of an R object.
## Preserving the state of an R object and making it available for future 
## computations can dramitacally improve the performance of the computation 
## assuming the state of the object does not change over time. For this example
## the user first creates a square matrix. For example y<- matrix(1:4, 2, 2)
## the user then passess the matrix (y) as an argumnetto the makeChacheMatrix  
## function and assings the result to another variable, for example yy. Thus the 
##operation would be yy <- cacheMatrixMean(y). The user can then get the inverse
## of yy and set the inverse of yy using the getinverse and set inverse function. 
## Finally the user can test to see if the inverse of the Martix y is saved for 
## use outside of the function makeCacheMatrix by using the function cacheSolve
## and passing it y as an argument.

## The makeCacheMatrix function takes a square matrix as an argument and saves the
## matrix in cache memory uses the <<- operator

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        
        get <- function() x
        setinverse <- function(solve) m <<- solve ##using <<- to cache matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function test to see if the inverese of the function is already saved in 
## the variable m and returns m if it is, else it computes the inverse of the
## matrix and returns it.
## 

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) { ## checking to see if the cached variable m has a value 
                        ## or is null
                message("getting cached data") ##message to end user
                return(m)
}
        data <- x$get() ## if m is null the get the matrix 
        m <- solve(data, ...) ## and set the inverse to m
        x$setinverse(m)
        m
}      

