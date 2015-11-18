## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        M <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) M <<- solve
        getsolve <- function() M
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

#For any matrix X
#we set a <- makeCacheMatrix()
#we can put a matrix X in the cache typing : a$set(X)
#We can then obtain the matrix : a$get()
# To solve for the first time and put into the cache the invert of the matrix :
# a$setsolve(solve(X))


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        M <- x$getsolve()
        if(!is.null(M)) {
                message("getting cached data")
                return(M)
        }
        data <- x$get()
        M <- solve(data, ...)
        x$setsolve(M)
        M
}

#cacheSolve(a) returns :
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#which is the expected result
