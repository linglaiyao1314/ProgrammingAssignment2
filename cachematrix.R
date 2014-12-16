## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    #set the value of our matrix
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    #use the get function to get our matrix
    get <- function() x
    
    #set the inverse matrix and store it in variable inverse
    setinverse <- function(inma) inverse <<- inma
    getinverse <- function() inverse
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()  
    #if the inverse matrix not null then return it 
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    #store the original matrix
    data <- x$get()
    #caculate the inverse matrix of the data
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
