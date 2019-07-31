## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object and can cache this matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set<- function(y){
                x<<-y
                invX<<-NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) invX <<- inverse
        getinverse <- function() invX
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        

}


## Write a short comment describing this function
## This function returns the inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getinverse()
        if(!is.null(invX)){
                return(invX)
        }
        data <- x$get()
        invX <- solve(data, ...)
        x$setinverse(invX)
        invX
                
                
}
