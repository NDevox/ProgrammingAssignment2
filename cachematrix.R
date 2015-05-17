## These functions are designed to create cached matrices (matrices stored in memory)
## Which have their inverses worked out and stored alongside them.
## To do this we make a list of functions and store them in variables.
## This list of functions contains a matrix, it's inverse,
## and a method to change their values.
## makeCacheMatrix creates the lists and gives an initial matrix.
## cacheSolve takes the list and provides it with the inverse matrix to complete it.

## makeCacheMatrix is a function which returns a list of functions.
## These functions hold the matrix we are using (which is retrievable using get)
## They also give us the ability to set the inverse of the matrix (set_inverse)
## Return the inverse once set (get_inverse)
## And allows us to change the matrix to be use (set)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
   
    ## Alows us to reset x to something of our choosing.
    ## And restores i to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get returns the original matrix
    get <- function() x 
    
    ## set_inverse allows us to set the inverse of the function
    set_inverse <- function(solve) i <<- solve
    
    ## get_inverse returns the inverse matrix
    get_inverse <- function() i
    
    ## Pass the list of functions
    return(list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse))
}

## cacheSolve sets the inverse of the cached matrix if it doesn't exists.
## If it does exist it simply returns the inverse of the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Get the inverse of the matrix, if it exists
    i <- x$get_inverse()
    
    ## If the inverse exists already, return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If the inverse doesn't exist, retrieve the matrix to be inverted
    data <- x$get()
    
    ## Invert the matrix using solve
    i <- solve(data, ...)
    
    ## Set the inverse in the function list for later use.
    x$set_inverse(i)
    
    ## Return the inverse.
    return(i)
}
