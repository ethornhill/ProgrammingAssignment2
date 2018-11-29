## Calculate and cache the inverse of a user inputed matrix if it has not already been cached

## Function 1, makeCacheMatrix, performs 4 functions 1. Sets the matrix
## 2. Gets the matrix 3. sets the inverse of the matrix 4. gets the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }

## Function 2, cacheSolve, solves for the inverse of the matrix if the inverse 
## of the matrix has not yet already been solved and cached. If it has already
## been solved then it retrieved the inverse from the cache. If it hasn't been
## solved it will calculate it and then cache it using the setinverse function

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }

