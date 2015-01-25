# When called on, this function will create an unique environment which will
# set a user inputted matrix, and have functions that store and calculate
# the matrix and it's inverse. This environment will allow the later function to 
# access the variables and functions created here. 

makeCacheMatrix <- function (x = matrix()){
   
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


# My cacheSolve function will retrieve the value of the stored inverse matrix
# value and determine if it is indeed an inverse of the original matrix. If it
# is already an inverse, then the function will return the calculated inverse
# instead of calculating another inverse. If not, then this function will
# produce the inverse.
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
    
    
    
}