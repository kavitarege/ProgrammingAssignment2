# In this assignment we are computing inverse of a matrix, and cache the output instead of computing it
# repeatedly, for this we create two functions makeCacheMatrix and cacheSolve.
# The Inverse of A is A^-1 only when A × A^-1 = A^-1 × A = I
# The matrix should be a Square Matrix(Number of rows = number of columns)
#input  example:xx <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
#Task 1
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#Assumption: The matrix supplied is always invertable
# The makeCacheMatrix takes a square matrix as  as input.

makeCacheMatrix <- function(x = matrix()) {
    # m is variable in which inverse of matrix is stored, initilized to NULL.
    m <- NULL
    #This function accepts new square matrix for which inverse has to be calculated
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #'get' function will fetch the matrix set by 'set' function.
    get <- function() x 
    # This function will store the computed inverse of matrix in  m
    setinv <- function(solve) m <<- solve
    # The 'getinv' function will fetch and diaplay the inverse of the given matrix from m 
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}
#Task 2
#cacheSolve: This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
#input example: cacheSolve(xx)

cacheSolve <- function(x, ...) {
    ## try to fetch the inverse of matrix, if it is already computed
    m <- x$getinv()
    #checks if the inverse is cached , if present fetches and returns the inverse value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #if not in cache, computes the inverse of matrix x, solve(data,...) computes the inverse of matrix.
    data <- x$get()
    m <- solve(data, ...)
    #store the inverse using setinv function 
    x$setinv(m)
    #Display the inverse of matrix x.
    m
}

