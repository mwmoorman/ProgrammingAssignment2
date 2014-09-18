## These are the functions created for the R Programming course's second programming
## assignment. These functions are modified versions of the sample code provided that
## will produce an inverse matrix.

## The makeCacheMatrix function is the first of the functions and allows us to create
## the necessary variables and functions that will be used in the cacheSolve function.
## To use this assign the output of this function to a variable that you will then input 
## into cacheSolve.


makeCacheMatrix <- function(x = matrix()) {
    cachedinverse<-NULL
    set<-function(y){
         x<<-y
         cachedinverse<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) cachedinverse<<-inverse
    getinverse<-function() cachedinverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function accepts data from the makeCacheMatrix function and outputs an inverse
## matrix. Simply runt this function on  the variable you created using makeCacheMatrix. int

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     cachedinverse <- x$getinverse()
    if(!is.null(cachedinverse)) {
      message("getting cached data")
      return(cachedinverse)
    }
    data <- x$get()
    cachedinverse <- solve(data, ...)
    x$setinverse(cachedinverse)
    cachedinverse

}
