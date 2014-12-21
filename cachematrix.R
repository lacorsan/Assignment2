## These functions calculate the inverse of a given matrix.

## MakeCacheMatrix is a function that creates an special object, this function in a first step sets the value of a matrix, in a second step get the value of that matrix, the third step consists on setting the value of the inverse, and finally gets the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    invmt <- NULL
	##this sets the value of the matrix
    set <- function(y) {
        x <<- y
        invmt <<- NULL
    }
	##this gets the value of the matrix
    get <- function() x
	## this sets the value of the inverse
    setinverse <- function(inverse) invmt <<- inverse
	##this gets the value of the inverse
    getinverse <- function() invmt
	##using this below we will obtain the matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that calculates the inverse matrix obtained by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
	##this returns the message if the inverse is calculated
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
	## we calculate the inverse if it is not calculated yet
  data <- x$get()
  im <- solve(data) #this finds the inverse matrix
  x$setinverse(im)  #cache the inverse
  im
}

##The code can be tested using the following lines:
##This first line creates a 2x2 matrix called (x):
## x <- rbind(c(1,2),c(3,4))
##Now, we are going to create the special object using makeCacheMatrix
## object <- makeCacheMatrix(x)
##Finally, we calculate the inverse of the special object 
## cacheSolve(object)