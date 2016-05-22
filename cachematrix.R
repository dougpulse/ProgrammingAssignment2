## Put comments here that give an overall description of what your
## functions do

##  Author:     Doug Pulse
##  Credit:     Structure provided by Roger Peng's makeVector and cachemean functions.

##  Testing info:
##      source("cachematrix.R")
##      a <- matrix(sample(1:10, 16, replace=T), 4, 4)
##      b <- makeCacheMatrix(a)
##      a
##      b$get()
##      b$getsolve()
##      cacheSolve(b)   ##  no message
##      cacheSolve(b)   ##  message indates cached data was used

##  function:   makeCacheMatrix
##  purpose:    creates a list containing the matrix and, if available, it's inverse.
##  input:      matrix x
##  output:     list(set, get, setsolve, getsolve)


makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    #   define the set member to allow the user to set the matrix to a new value
    #   This should also destroy the cached inverse.
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    #   define the get member to allow the user to get the current value of the matrix
    get <- function() x
    
    #   define the setsolve member to allow the user to calculate the inverse of the matrix and store it
    setsolve <- function(solve) s <<- solve
    
    #   define the getsolve member to allow the user to retrieve the inverse of the matrix rather than calculate it
    getsolve <- function() s
    
    #   return the list of object members
    #   This is the makeCacheMatrix object as presented to the user.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


##  function:   cacheSolve
##  purpose:    Provide a means to return the inverse of a matrix from a cache, if available.
##  input:      makeCacheMatrix x
##              other arguments the user wants to pass to the solve() function
##  output:     The inverse of the matrix contained in x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    
    ##  Was a value found in the cache?
    if(!is.null(s)) {
        ##  Yes.  Return the cached value.
        message("getting cached data")
        return(s)
        ##  function returns -- code below here is skipped
    }
    
    ##  The cache didn't contain the inverse.
    ##  Pass the matrix (and other arguments) to the solve() function...
    data <- x$get()
    s <- solve(data, ...)
    
    ##  ...write the results to the cache...
    x$setsolve(s)
    
    ##  ...and output inverse of the matrix.
    s
}