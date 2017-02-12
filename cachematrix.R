## In this programming assignment, we shall write two functions makeCacheMatrix and cacheSolve
## Their purpose is to make time consuming computations easy.
## We will use lexical scoping in this assignment to get the inverse of a matrix
## Lexical scoping is particularly useful for simplifying statistical computations. 
## Programs using lexical scoping have 'free variables' that are searched for in the environment in which the function is defined.

## Note: In this example we assume the matrix supplied is invertible. 


## makeCacheMatrix is a user defined special vector


makeCacheMatrix <- function(x = matrix()) {  ## initializing function argument as an empty matrix
        matrix_inverse <- NULL ## NULL is a reserved word & using it to initialize the object within the function environment
        set <- function(y) { ## sets the value of the matrix
                x <<- y ## assigns y to object x in the parent environment
                matrix_inverse <<- NULL
        }
        get <- function() x ## gets the value of x from the parent environment: 'makeCacheMatrix'
        set_inverse <- function(inverse) matrix_inverse <<- inverse ## sets the value of inverse of matrix
        get_inverse <- function() matrix_inverse ## gets the value of inverse of the matrix
        list(set = set, get = get,     ## assigns the elements to a list and returned to the parent environment 
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## cacheSolve function computes the inverse of the `makeCacheMatrix` above. 
## It first checks if the inverse is calculated and the matrix has not changed, then returns the result
## Otherwise calculates the inverse and sets the value via the 'set_inverse' function

cacheSolve <- function(x, ...) {
        
        matrix_inverse <- x$get_inverse()
                                                ## command to check
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        
       
        data <- x$get()
        matrix_inverse <- solve(data, ...) ## solves matrix multiplication (% * %)
        x$set_inverse(matrix_inverse)           ## calculates and returns the result
        matrix_inverse
}
