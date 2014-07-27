## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
        n <- NULL
        set <- function(l) {
                m <<- l
                n <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {       
        n <- x$getinverse()
        if( !is.null(n) ) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data) %*% data
        x$setinverse(n)
        n
}
