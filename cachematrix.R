## There are two objects that are required at a conceptual level  
## One of the objects computes the inverse of a matrix. This would use a  function to compute the inverse of a matrix. 
## If we have a matrix called "matrix_item" then the function "solve(matrix_item)" should give the inverse.
## Perhaps this conceptual object can be represented by a physical object x that gets the inverse of the matrix. 
## The second conceptual object should cache the result obtained from x.
## Finally, there should be approprite functions to link these objects.
## This function deals with the cacheing of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y){
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inverse_matrix <<- solveMatrix
        getInverse <- function() inverse_matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function will compute the inverse of a matrix through the function of the nature solve(matrix_item)

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getInverse()
        if(!is.null(inverse_matrix)){
                message("getting cached data")
                return(inverse_matrix)
        }
        matrix_item <- x$get()
        inverse_matrix <- solve(matrix_item)
        x$setInverse(inverse_matrix)
        inverse_matrix      
}
