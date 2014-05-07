## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        
        get <- function() x
        
        setinverse <- function(inv) inverse <<- inv
        
        getinverse <- function() inverse
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse
        if(!is.null(inverse)){
                message("getting cached inverse matrix")
                return(inverse)
        }
        matrix <-x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}
