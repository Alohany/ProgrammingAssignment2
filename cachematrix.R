## These functions allow the user to calculate the inverse of a matrix using the R solve()
## function and then cache the inverse for subsequent retrieval.

## The functions assume that the matrix is invertible and has not changed over time.  The
## functions reduce the processing overhead of repeatedly calculating the inverse of the same 
## function.

## This function constructs a "special" vector containing four functions that:
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the inverse of the matrix
## 4 - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m<-NULL
    
    set <- function (y) {
        
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m<<-inverse
    
    getinverse <- function() m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## This function checks to see if the inverse of the matrix has already been cached
## If it has been, the inverse is returned.  If not, the inverse is calculated using
## the solve function and then stored for future retrieval

cacheSolve <- function(x,...) {
    
    l <- makeCacheMatrix(x)
    
    m<-l$getinverse()
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data<-l$get()
    
    m<-solve(data,...)
    
    l$setinverse(m)
    
    m
}