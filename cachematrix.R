## The following code would introduce a method to cache the inversed matrix,
## whereas necessary, and return the cached version directly if it exists.
## The actual inverse operatin would occur only once when it hits the 1st time

## makeCacheMatrix
## Create a special matrix that embeds an inversed version of itself

## Arguments:
## x - the input matrix, defaults to an empty matrix
## Return: 
## The special matrix with its inversed version cached, whereas necessary

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    setInverse <- function(y){
        inverse <<- y
    }
    
    getInverse <- function(){
        inverse
    }
    
    getData <- function(){
        x
    }
    
    setData <- function(z){
        x <<- z
        inverse <<- NULL
    }
    
    list(setData = setData, getData = getData, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve
## Inverse the specified matrix

## Arguments:
## x - the matrix to be inversed
## Return:
## The inversed matrix

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    ret <- x$getInverse()
    if(is.null(ret)){
        data <- x$getData()
        ret <- solve(data)
        x$setInverse(ret)
    }
    
    return(ret)
}
