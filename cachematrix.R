##Hamel Husain Coursera R Programming Assignment 2
## These functions help cache the inverse of a matrix rather than computing it repeatedly


###############################################################################
## This function creates a special "matrix" object that can cache its inverse.
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
        #intializes the inverse to a null value
        i <- NULL
        
        #set the value of the matrix, and intialize the inverse to null
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #get the values of the matrix
        get <- function() x
        
        #set the value of the inverse if you know what it is
        setinverse<- function(inverse) i <<- inverse
        
        #get the cached inverse of the matrix
        getinverse <- function() i
        list(set = set, get = get,
             setmean = setinverse,
             getmean = getinverse)
}

###############################################################################
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  then cacheSolve should retrieve the inverse from the cache.
###############################################################################
cacheSolve <- function(x, ...) {
        
        ##First attempt to get cached inverse if exists already
        i <- x$getinverse()
        
        ##If cached inverse exists, then return the value and we are done
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ##Otherwise, if cached inverse does NOT exist, then retrieve the matrix 
        # value and compute the inverse of the matrix
        matrix_value <- x$get()
        i <- solve(matrix_value, ...)
        
        #Place the inverse matrix into the cache
        x$setinverse(i)
        
        #Return the inverse
        i
}
}
