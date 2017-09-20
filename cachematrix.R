## Put comments here that give an overall description of what your
## functions do
## the purpose of the function is to calculate the inverse of a matrix and
## save the result to the cache so that when the user want to calculate the 
## matrix inverse, the previously saved value is return rather than repeat 
## the calculation

## Write a short comment describing this function
## the function, "makeCacheMatrix" creates a special "matrix", which is 
## really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
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
## The following function calculate the inverse of the special
## "matrix" created with the above function. It first checks to 
## see if the inverse has already been calculate. If so, it gets
## the inverse from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value
## of the inverse via the setinverse function
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m 
     ## Return a matrix that is the inverse of 'x'
}
