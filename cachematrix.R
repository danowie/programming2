## this function should create a special matrix obhject that caches its inverse

## This first function will...
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL 
	}	
	get <- function(x)
	setinverse <<- function(inverse) inv <<- inverse
	getinverse <<- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function will cache the inverse of the special matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        getinverse <- x$getinverse()
        if(!is.null(inv)) {
        	message ("retrieving cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

## I will run them in R studio and see what happens...

##> source("makecachematrix.R")
##> source("cachesolve.R")

## x <- rbind(c(1, 2), c(2,1))
## > x
##     [,1] [,2]
## [1,]    1    2
## [2,]    2    1
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
## retrieving cached data
##           [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333