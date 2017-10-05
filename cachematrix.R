## Put comments here that give an overall description of what your
## functions do

# Two functions that allow to inverse matrices, cache the result, and retrieve the result shouldinversed matrix, should it already exist in the cache.


## Write a short comment describing this function

# This function transforms a passed matrix into a list of functions that allow modification of the matrix, as well as
# storage and retrieval of the modifications (inversion in this particular excercise).

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL                                                  
    set <- function(y) {
        x <<- y                                                     # set the matrix
        invmat <<- NULL                                             # set the cache of the inverse matrix to NULL
    }
    get <- function() x                                             # get the passed matrix
    setinvmat <- function(inverse) invmat <<- inverse               # set the cache of the inverse matrix
    getinvmat <- function() invmat                                  # get the cache of the inverse matrix
    list(set=set,get=get,setinvmat=setinvmat,getinvmat=getinvmat)   # return a list with the functions
}


## Write a short comment describing this function

# This function takes a list generetad by makeCacheMatrix, calculates the inverse of the matrix passed to makeCacheMatrix,
# stores and returns the inverse. Should an inversed matrix already exist in the environment of the list passed list,
# the cached matrix is retrieved and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinvmat()                            # get the cached inversed matrix from the environment of the passed list
    if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)                         # return the cached inversed matrix if it exists
    }
    data <- x$get()                                    # get the matrix passed to makeCacheMatrix
    invmat <- solve(data, ...)                         # inverse said matrix
    x$setinvmat(invmat)                                # store the inversed matrix in the environment of the passed list
    invmat                                             # return the inversed matrix
}


## Test the functions
test<-matrix(runif(16),ncol=4)
solve(test)   # required outcome

x<-makeCacheMatrix(test)
cacheSolve(x) # actual outcome (matches)
