## Put comments here that give an overall description of what your
## functions do

# Here we are trying to write a pair of functions which enables us to
# have a special kind of matrix which can cache its inverse. This allows 
# us not to compute the inverse again and again, thus saving computational power


## Write a short comment describing this function

#makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#this function takes a matrix as an input and returns a list of four functions:
#get and set functions allow us to get and set original matrix inside the new specialized
#matrix object. getinverse and setinverse functions do the same for inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        
        set <- function(y){
                x<<-y
                inverse<<-NULL
        }
        
        get <- function() x
        
        setinverse <- function(inv) inverse <<- inv
        
        getinverse <- function() inverse
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

#this function takes our specialized cachematrix as input and returns the inverse matrix.
#it first checks if the inverse is already cached. If not then it calculates the inverse,
#puts it in the specialized cachematrix and returns it as well. If its already cached, it
#just returns the cached inverse matrix instead of computing it again
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
