## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix  creates a special "matrix"
## This is really a list containing a function to 
## 1.	set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the value of the inverse of the matrix
## 4.	get the value of the inverse of the matrix


## Code structure follows the sample in instructions

# Assumption: the matrix is square and invertible
# No checks done for input errors


makeCacheMatrix <- function(mtrx = matrix()) {

	invM <- NULL  # The inverse matrix
	
	# If the matrix  value is set, set the inverse to NULL
	set <- function(y) {
			mtrx <<- y
			invM <<- NULL
		   }
		   
	get <- function()  { 
		mtrx
		}
        
	setInverse <- function(myInverse)  {
					invM <<-  myInverse
				}
					
    getInverse <- function() {
					invM
					}
					
        list(set = set,
			 get = get,
             setInverse = setInverse,
             getInverse = getInverse)
	   
		   
		   


}


## Write a short comment describing this function

## cacheSolve() calculates the mean of the special "matrix" 
## created with makeCacheMatrix(), above
##
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse  of the matrix, and
##  sets the value of the inverse matrix  in the cache via the setInverse function



cacheSolve <- function(mtrx, ...) {
        ## Return a matrix that is the inverse of 'mtrx'
		
		invM <- mtrx$getInverse()
        if (!is.null(invM)) {
            message("getting cached data")
            return(invM)
		}
		
		data <- mtrx$get()
        mInv <- solve(data, ...)
        mtrx$setInverse(mInv)
        mInv


}

