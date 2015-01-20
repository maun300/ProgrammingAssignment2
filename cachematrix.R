## These functions is used to cache a matrix with it's inverse

## Creates a list with functions to manage a cached matrix with it's inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<-inverse
    getInverse <- function() i
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'. The function makeCacheMatrix
## should be used to create 'x'.
## The function first checks if the inverse has already been calculated
## If not is the inverse calculated and stored in the cache for future 
## references
cacheSolve <- function(x, ...) {
   i <- x$getInverse()
   if(!is.null(i)) {
       message("Getting cached data.")
       return(i)
   }
   
   data <- x$get()
   i <- solve(data, ...)
   x$setInverse(i)
   i
}
