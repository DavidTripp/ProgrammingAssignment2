## Put comments here that give an overall description of what your
## functions do

## Function Names: cacheSolve, makeCacheMatrix
#
## Author:         David Tripp 
##                   {for the Coursera Johns Hopkins R Programming course}
#
## Purpose:        Return the inverse of a matrix passed as input using the R solve()
##                 function, but caching previously calculated inverse matrices to 
##                 minimize unneeded computational overhead. This is accomplished by
##                 use of makeCacheMAtrix and cacheSolve in conjunction with each other 
##                 as shown in the usage example below.
#
## Usage Example:
#
##    *** Define a matrix wtith some values ***
##    > x = rbind(c(1, 9, 15), c(100, 33, 1), c(-1, -2, -3))
##
##    *** Pass the matrix to the solve function to calculate baseline values ***
##    > solve(x)
##    [,1]        [,2]      [,3]
##    [1,] -1.089888 -0.03370787 -5.460674
##    [2,]  3.359551  0.13483146 16.842697
##    [3,] -1.876404 -0.07865169 -9.741573
##
##    *** Assign m the value of makeCacheMatrix with our basline matrix as input ***
##    > m = makeCacheMatrix(x)
##
##    *** Verify the cached matrix has the same values as our baseline ***
##    > m$getMatrix()
##    [,1] [,2] [,3]
##    [1,]    1    9   15
##    [2,]  100   33    1
##    [3,]   -1   -2   -3
##
##    *** call cacheSolve to find the inverse, with the first call not being cached as expected ***
##    > cacheSolve(m)
##    [,1]        [,2]      [,3]
##    [1,] -1.089888 -0.03370787 -5.460674
##    [2,]  3.359551  0.13483146 16.842697
##    [3,] -1.876404 -0.07865169 -9.741573
##
##    *** call cacheSolve again with the result returned from cache as expected. ***
##    > cacheSolve(m)
##    getting cached data.
##    [,1]        [,2]      [,3]
##    [1,] -1.089888 -0.03370787 -5.460674
##    [2,]  3.359551  0.13483146 16.842697
##    [3,] -1.876404 -0.07865169 -9.741573
 
  
makeCacheMatrix <- function(inputMatrix = matrix()) {
  
        ## Initialize the inversMatrix variable to NULL.
        inverseMatrix <- NULL
        
        ## Create set function to allow resetting of the inputMatrix 
        ## vector without the overhead of reinstantiating the 
        ## makeCacheMatrix object.
        setMatrix <- function(setInputMatrix) {
                           inputMatrix    <-- setInputMatrix
                           inverseMatrix  <-- NULL
        }
        
        ## Method for getting the value of input matrix.
        getMatrix <- function() x
        
        ## Method for setting the value of the inverse of the input matrix.
        setInverse <- function(setInverseMatrix) inverseMatrix <<- setInverseMatrix
        
        ## Method for getting the value of the inverese of the input matrix.
        getInverse <- function() inverseMatrix
        
        list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(cachedMatrix, ...) {
  
        ## Initialize the inversMatrix variable to value in makeCacheMatrix
        ## object. NULL if not previously calculated else the previously 
        ## calculated value.
        inverseMatrix <- cachedMatrix$getInverse()
        
        ## If the value was previously calculated (ie not null) then display 
        ## a message indicating a cached value is being used and return that value.
        if(!is.null(inverseMatrix)) {
              message("getting cached data.")
              return(inverseMatrix)
        }
        
        ## Set the value of inputMatrix variable by calling the getMatrix method 
        ## of the cachedMatrix object provided as input.
        inputMatrix    <- cachedMatrix$getMatrix()
        
        ## Set the value of inverseMatrix variable by calling the solve function 
        ## using the previously populated inputMatrix variable as input.
        inverseMatrix  <- solve(inputMatrix)
        
        ## Set the value of inverseMatrix in the cachedMatrix object using the 
        ## setInverse method.  This is how the results are cached for subsequent
        ## calls for the inverse of a given matrix.
        cachedMatrix$setInverse(inverseMatrix)
        
        ## Return the inverseMatrix value.
        inverseMatrix
  
}
