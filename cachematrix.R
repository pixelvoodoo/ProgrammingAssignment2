## https://github.com/x
## 1st commit SHA-1 hash identifier: x
## R Programming Assignment 2: Lexical Scoping

## Code below is for Coursera assignment and contains two pairs of functions that cache the inverse of a matrix.

## Matrix inversion is usually a costly computation and there may be some benefit to caching ...
## ... the inverse of a matrix rather than computing it repeatedly. 

## Code was validated using guidance from Jules Stuifbergen in course forum
## https://class.coursera.org/rprog-011/forum/thread?thread_id=405

##This function creates a special "matrix" object that can cache its inverse.
##makeCacheMatrix works in four steps.
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        
        ##inv stands for inverse
        ##Incase of new values in the matrix best to reset the cache
        inv <- NULL
        
        ##Sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ##Gets the value of the matrix
        get <- function() x
        
        ##Sets the inverse value of the matrix
        ##The use of <<- is for...
        set_inverse <- function(solve) inv <<- solve
        
        ##Gets the inverse value of the matrix
        get_inverse <- function() inv
        
        ##Coerce the function value to a list
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), ...
## ... then cacheSolve retrieves the inverse from the cache.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        
        ##Checks if the inverse has aleady been calculated and if so returns the cached values
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##If the above logical statement for checking cache value returns NULL then the ...
        ## ... inverse of the matrix is calculated
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$set_inverse(inv)
        
        ##Returns inverse value
        inv        
        
}
