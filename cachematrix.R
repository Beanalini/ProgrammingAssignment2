############################################################################
#The following two functions, makeCacheMatrix() and cacheSolve() are used to 
#create an object which is able to cache the inverse of a square matrix. 
#It is assumed that the supplied matrix is always invertible.
###########################################################################

#The makeCacheMatrix() function builds a set of functions and returns the
#functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
      #Set inv to NULL
      inv <- NULL
      
      #define set function
      #Initialize y
      set <- function(y){
            
            #Set the value of x in the parent environment to y
            x <<- y
            #Set value of inv in the parent environment to Null
            inv <<- NULL
      }
      #Define the get function
      get <- function() {x}
      #Define the setInverse function
      setInverse <- function(inverse) {inv <<- inverse}
      #Define the setInverse function
      getInverse <- function() {inv}
      
      #Assign functions as named elements in a list and return it to the
      #parent environment (x and m are included as elements within the list).
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#The cachesolve() function populates and/or retrieves the inverse matrix from an   
#object of type MakeCacheMatrix(). If the inverse matrix has not yet been 
#calculated, or the input matrix has changed, cachesolve() calculates and caches the
#inverse matrix.


cacheSolve <- function(x, ...){ 
      #Retrieve cached inverse matrix 
      inv <- x$getInverse()
      #Check whether the inverse matrix exists
      if(!is.null(inv)){
            #A valid cache matrix exists. 
            message("getting cached data")
            #Return the cached inverse matrix to the parent environment
            return(inv)
      }
      #Input matrix new or has changed
      #Retrueve the matrix from the input object
      mat <- x$get()
      #calculate the inverse matrix
      inv <- solve(mat, ...)
      #cache the inverse matrix: Set the inverse matrix in the input object 
      x$setInverse(inv)
      #Return and print the inverse matrix to the parent environment
      inv
}

###################################################################
#Testing used to check functions work correctly:
# ################################################################
#
# >source("test.R")
# > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# > myMatrix_object <- makeCacheMatrix(m1)
# > cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4
# > m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
# 
# > myMatrix_object$set(m2)
# > cacheSolve(myMatrix_object)
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5
# > cacheSolve(myMatrix_object)
# getting cached data
# [,1] [,2]
# [1,]    3    7
# [2,]    1    5

