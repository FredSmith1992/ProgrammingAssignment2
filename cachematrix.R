# cachematrix.R

## cachematrix.R is the program we are to deliver for Programming Assignment 2 in Coursera's R Programming course.
## It is based on a similar set of caching functions providing in rdpeng's original project,
## which we forked from https://github.com/rdpeng/ProgrammingAssignment2 to create our own.

## it serves to teach us about using functions as arguments and storing data in another context or frame
## it has two primary functions, makeCacheMatrix() and cacheSolve(), which are used to get and set results

## makeCacheMatrix holds the previously calculated value, if any, and the functions to check for, save and return it

## makeCacheMatrix() is the context in which the previously calculated value, if any, and the functions to check for, save and return it

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## cacheSolve() uses the functions in makeCacheMatrix()'s context  to shortcut the calculation by using a previously saved value if available
## if it hadn't been previously performed, it performs the matrix inversion, which is done by calling solve().

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
 }
 
