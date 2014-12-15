## Objective: To weite a pair of functions that cache the inverse of a matrix.
##Author: Oscar Reyes
## makeCacheMatrix: creates a special "matrix" object that can cche its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setmatrixinverse <- function(inverse) m_inv <<- inverse
        getmatrixinverse <- function() m_inv
        list(set = set, get = get,
             setmatrixinverse = setmatrixinverse,
             getmatrixinverse = getmatrixinverse)
}


## cacheSolve: Computes the inverse of the special "matrix" retruned by makeCacheMatrix above.
##              If the inverse has already been calculated (and the matrix has not changed)
##              then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getmatrixinverse()
        if(!is.null(m_inv)) {
                message("getting cached data - inverse matrix")
                return(m_inv)
        }
        matrix <- x$get()
        m_inv <- solve(matrix, ...)
        x$setmatrixinverse(m_inv)
        m_inv
}
