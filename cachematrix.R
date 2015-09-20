
## R Programming Assignment 2 - Taylor Van Anne
## This script demonstrates how to cache a calculation, check if a calculation has been
## stored recently, and uses the cached calculation instead of recalculating

## This function takes in a matrix, sets the inverse placeholder to NULL, and
## holds the setinv and getinv functions
makeCacheMatrix <- function(x = matrix()) {
     invx <- NULL #default invx to null, this is where the inverse will be stored
     set <- function(y) { #function changes the matrix to be inverted
          x <<- y 
          invx <<- NULL
     }
     get <- function() { #returns the current uninverted matrix
          return(x)
     }
     setinv <- function(inverse) { #inverts the matrix
          invx <<- inverse
     }
     getinv <- function() { #gets the inverted matrix
          return(invx)
     }
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function tests to see if a value has been cached, if so, it will
## return that cached value. If not, it will calculate the inverse of the matrix and return
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invx <- x$getinv()
     if(!is.null(invx)) {
          message("getting cached data")
          return(invx)
     }
     message("first-time calculation")
     initmatrix <- x$get()
     invx <- solve(initmatrix, ...)
     x$setinv(invx)
     return(invx)
}

#***************************************  MATRIX TEST DATA ***************************
#build a test matrix
mymatrix <- matrix(nrow = 3, ncol = 3)
numbers <- c(1, 0, 5) 
numbers2 <-  c(2, 1, 6)
numbers3 <- c(3, 5, 0)
mymatrix[,1] <- numbers
mymatrix[,2] <- numbers2
mymatrix[,3] <- numbers3

#view the matrix before it is passed to the function
mymatrix

#Test is the new object of formulas 
test <- makeCacheMatrix(mymatrix)
test$get()
test$set(mymatrix)

#the first time this runs, it will calculate. 
cacheSolve(test)
#the second time this runs, it will use the cached version.
cacheSolve(test)
