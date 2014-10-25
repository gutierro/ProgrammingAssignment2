
## It creates an object with 4 functions and 2 variables (matrix and the inverse).
makeCacheMatrix <- function(x = matrix()) {
        
        ## initializating the inverse.
        m <- NULL
        
        ## it sets the matrix object 'x' and initializes the inverse matrix 'm' (in the parent environment).
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## it gets the matrix object 'x'
        get <- function() x
        
        ## it sets the inverse matrix 'm' (in the parent environment).
        setInverse <- function(inverse) m <<- inverse
        
        ## it gets the inverse matrix 'm'.
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)       
}


## It returns the inverse of the special matrix object created with the function makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        ## it gets the inverse from the cached object 'x' (special matrix) and checks if the inverse object 'x' is empty..
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If the inverse variable in the cached object 'x' is empty, 
        ## it gets the matrix value from object 'x' 
        data <- x$get()
        
        ## it calculates the inverse and save it in the object 'x' for next time .
        m <- solve(data)
        x$setInverse(m)
        m
}

## Example run:
## > mdat<-matrix(c(1 ,1, 1, 1, 1, 2, 1, 2 ,1, 1, 1, 0 ,1, 4, 2, 3),nrow=4,ncol=4)
## > m = makeCacheMatrix(mdat)
# > m$get()
# [,1] [,2] [,3] [,4]
# [1,]    1    1    1    1
# [2,]    1    2    1    4
# [3,]    1    1    1    2
# [4,]    1    2    0    3
# > cacheSolve(m)
# [,1] [,2] [,3] [,4]
# [1,]   -1   -2    3    1
# [2,]    2    1   -3    0
# [3,]    1    1   -1   -1
# [4,]   -1    0    1    0
