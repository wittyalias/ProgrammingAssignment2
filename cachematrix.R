## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     
     #This creates the variable that will store the inverse matrix
     inv <- NULL
     
     #this function assigns y to to be the matrix and clears the stored inverse
     set <- function (y){
          x <<- y
          inv <<- matrix()
     }
     
     #The get function simply returns the matrix
     get <- function() x
     
     # The setinv function assigns the inverse matrix
     setinv <- function(inverse) inv <<- inverse
     
     # The getinv function returns the inverse of the matrix
     getinv <- function() inv
     
     #This returs a list of the above functions if 
     list(set = set, get = get, setinv =setinv, getinv = getinv)
}

##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     inv <<- x$getinv()  
     
     #This tests whether the inverse exists. If it is not NULL then it has already been solved and 
     # the matrix hasn't been changed.
     if(!is.null(inv)){
          message("Inverse previously calculated")
          return(inv)
     }
     
     # If the invers is NULL then it hasn't been solved, or the matrix has been changed, meaning it needs to 
     # be solved and re-cached, which is done by the code below.
     
     # This code gets the matrix, then solves it and sets the inv variable to the solution.
     mat <- x$get()
     inv <- solve(mat)
     
     # This code caches the inverse matrix and then returns it
     x$setinv(inv)
     inv
     
}
