## Put comments here that give an overall description of what your
## functions do

## the functions calculate the inverse of a matrix if that matrix has not already
## been calculated.  if it has already been calculated the cache'd inverse is
## returned.

## Write a short comment describing this function

## the purpose of makeCacheMatrix is to return an object containing the four
## functions.  The four functions simply return values to cacheSolve and enable
## the actual cache created in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    
    #when set function is called it initializes x and m.  x and m are global
    #variables kept in the cache.
    set<- function(y) {
        x<<-y
        m<<-NULL
    }
    
    #get is a function that just returns x
    get <- function() x
    
    #set.solve is a function that takes solve as an argument and assigns it
    #to m.
    set.solve <- function(solve) m <<- solve
    
    #get.solve is a function that returns m, matrix from set.solve
    get.solve <- function() m
    
    #the function returns an object list of functions.
    list(set = set, get = get,
         set.solve = set.solve,
         get.solve = get.solve)
}


## Write a short comment describing this function

## cacheSolve either calculates an inverse matrix or returns the cache'd inverse
## created in a previous call of cacheSolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #m gets the global variable m produced by the get.solve function in the x object
    m <- x$get.solve()
    
    #if m is not null, i.e. it has been defined as a matrix, we get the cached m
    #and the cacheSolve function ends
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #if m is still NULL, data gets the cached x matrix from the get function
    data <- x$get()
    
    #m gets the inverse of x
    m <- solve(data, ...)
    
    #m is defined globally through the set.solve function
    x$set.solve(m)
    
    #the function returns m
    m
}
