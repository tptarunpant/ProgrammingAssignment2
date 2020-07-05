## The following functions create a matrix object, calculate its inverse ,cache its inverse and retrieve it

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { #Function to set the value of matrix in a non-local environment variable
                x <<- y
                i <<- NULL
        }
        get <- function() x #Function to get the value of matrix from a non-local environment variable
        setinv <- function(inv) i <<- inv #Function to set the inverse value of matrix 
        getinv <- function() i #Function to get the inverse value of matrix 
        list(set = set, get = get, #Returns list of functions
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()#Get the inverse value of matrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i) #Return the inverse of matrix from the cache given it is not already cacluated
        }
        data <- x$get()
        i <- solve(data, ...) #Calculate the inverse of the matrix using the solve function
        x$setinv(i)
        i
}
