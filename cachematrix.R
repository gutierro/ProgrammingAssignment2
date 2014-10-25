
## Funtion to create the matrix object.
## This i
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse in the current environment
        m <- NULL
        
        ## Funtion to set the Matrix and to initialize the inverse matrix 'm'.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Funtion to get the Matrix
        get <- function() x
        
        ## Funtion to set the inverse matrix 'm'.
        setInverse <- function(inverse) m <<- inverse
        
        ## Get the inverse matrix 'm'.
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)       
}


## It returns the inverse of the special matrix created with the function object makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        ## Gets the inverse from the cached object 'x' and checks if not empty inverser.
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If the cached object 'x' doesn't have and inverse then calculates the inverse and save it in the object 'x'.
        data <- x$get()
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
