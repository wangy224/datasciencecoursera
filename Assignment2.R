makeCacheMatrix <- function (x = matrix()){
    # This function is created so that when it is later called, the environment
    # in this function will the unique environment accessed for caching
    
    s <- NULL
    # for the later "<<-" search
    
    set <- function (y){
        x <<- y
        s <<- NULL
        # From what I've read on discussion boards and on google, "<<-" is 
        # necessary so that these variables will look to the parent frame,
        # instead of assigning the variable to what's in the global envir.
        # This is here to set a new matrix as needed.
    }
    get <- function() x
    # This will store our matrix
    
    setinverse <- function(solve) s <<- solve
    # This will set the inverse of our matrix 
    
    getinverse <- function() s
    # This will store our inversed matrix for later retrieval
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    # These are the input parameters of this function that will be called later
}

cacheSolve <- function(x){
    
    s <- x$getinverse()
    # Get our inverse matrix from the makeCacheMatrix environment
    
    if(!is.null(s)){
        message("getting cached data")
        return(s)
        # If Matrix is already an inverse, then use cached data and print it
    }
    data <- x$get()
    # This retrieves the matrix from the makeCacheMatrix environment
    
    s <- solve(data)
    # Now we find the inverse of the retrieved matrix
    
    x$setinverse(s)
    # Now we store that value
    
    s  # This prints it out our new inversed matrix
    # All this will invertmatrix from the parent environment of MakeCacheMatrix
    # if it is not inverted already
    
    
}