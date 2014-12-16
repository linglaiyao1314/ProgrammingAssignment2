## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #逆矩阵初始化
    inverse <- NULL 
    #设置矩阵的值
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    #获取矩阵的值
    get <- function() x
    #设置逆矩阵的值
    setinverse <- function(inma) inverse <<- inma
    #获取逆矩阵的值
    getinverse <- function() inverse
    #返回函数列表
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    #取逆矩阵的值
    inverse <- x$getinverse()  
    #若非null值，则直接返回结果
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    #计算其逆矩阵
    inverse <- solve(x$get())
    #设置逆矩阵结果
    x$setinverse(inverse)
    #返回逆矩阵
    inverse
}
