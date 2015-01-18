# This is cachematrix.R

## Put comments here that give an overall description of what your
## functions do

#  The makeCacheMatrix function creates a special "matrix" object that can cache
#  its inverse.  

#  The cacheSolve function computes the inverse of the special matrix returned 
#  by the makeCacheMatrix function.  If the inverse has already been calculated 
#  AND the matrix hasn't changed, this function retrieves the inverse from the 
#  cache rather than recalculate it again.

#  Usage example:  
#    my_matrix <- matrix(c(1, -1/4,-1/4,1), nrow=2)
#    useList <- makeCacheMatrix(my_matrix)
#    cacheSolve(useList)


## Write a short comment describing this function

#  The makeCacheMatrix function assumes that the supplied matrix is always 
#  invertible.  The makeCacheMatrix function creates a list containing a 
#  function to set the value of the matrix, get the value of the matrix, 
#  set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

#  The cacheSolve function calculates the inverse of the special "matrix" 
#  created with the makeCacheMatrix function.  But first, it checks to see if 
#  the inverse has already been calculated.  If so, it gets the inverse from the 
#  cache and skips the calculation.  Otherwise, it calculates the inverse of the
#  matrix and sets the value of the matrix in the cache via the setinverse 
#  function.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

