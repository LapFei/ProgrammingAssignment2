## Put comments here that give an overall description of what your
## functions do
## The functions can inverse a matric and cache it after it is calculated. It consist of two parts.
## 1. makeCachMatrix. This functions will let the programmer to set the matrix to be inversed and 
## cache it in the environment of the function. It will also cache the inversed mactrix resulted from
## cacheSolve to i.
## 2. cacheSolve will check wether the cache (inversed matrix) is exist and will get the what 
## is stored without recompute it. If the cache is empty, it will inverse the matrix with function
## 'solve' and then cache the result in the i of makeCacheMatrix.

## This function is the first part of the functional unit for caching and calculating
## a inverse matrix. It's return is the argurment for the second function.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y){
        x <<- y
        i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will check wether the matrix has been inversed and stored. If so, it will retrieve
## the copy in the cache. If not, it will inverse the matrix and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message('getting cached data')
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

