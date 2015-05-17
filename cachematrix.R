# cachematrix.R
#
#(c)2015 FredSmith1992 (at least, on github!)

# cachematrix.R is the program we are to deliver for Programming Assignment 2 in Coursera's R Programming course.
# It is based on a similar set of caching functions providing in rdpeng's original project,
# which we forked from https://github.com/rdpeng/ProgrammingAssignment2 to create our own.

# The purpose of this assignment is to teach us about using functions as arguments and storing data in another context.

# Usage:
# 1) Execute this script to create the functions and their environments
# 2) create a matrix and store it. We will assume it is called matrixOne for purposes of this usage explanation
#    > matrixOne<-v1<-cbind(c(1,2,3),c(23,77,91),c(15,19,53))
# 3) set the environment  by calling makeCacheMatrix with matrixOne; I'm saving it in m1c, here:
#    > m1c<-makeCacheMatrix(matrixOne)
# 4) The first time you call cacheSolve() it will perform the matrix inversion and return it.
# > cacheSolve(m1c)
#      [,1]        [,2]        [,3]
# [1,]  4.8  0.29795918 -1.46530612
# [2,] -0.1  0.01632653  0.02244898
# [3,] -0.1 -0.04489796  0.06326531
# >
#
# 5) On subsequent calls to cacheSolve(), it displays "getting cached data" and returns the cached result, saving time
# > cacheSolve(m1c)
# getting cached data
#      [,1]        [,2]        [,3]
# [1,]  4.8  0.29795918 -1.46530612
# [2,] -0.1  0.01632653  0.02244898
# [3,] -0.1 -0.04489796  0.06326531
# >

# This script has two primary functions, makeCacheMatrix() and cacheSolve(), which are used to set and get results

# makeCacheMatrix holds the previously calculated value, if any, and the functions to check for, save and return it
# makeCacheMatrix() is the context in which the previously calculated value, if any, and the functions to check for, save and return it

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
