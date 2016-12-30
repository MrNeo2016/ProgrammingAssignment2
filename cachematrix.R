#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
#These pair of functions caches the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
    m_1 <- NULL
    set <- function(y) {
        x <<- y
        m_1 <<- NULL
}
get <- function() x
setinwersja <- function(inwersja) m_1 <<- inwersja
getinwersja <- function() m_1
list(set= set, get= get, setinwersja= setinwersja, getinwersja= getinwersja)
}
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
 m_1 <- x$getinwersja()
 if(!is.null(m_1))
 {     message("getting_cached_data")
  return(m_1)
 }
 data <- x$get()
 m_1 <- solve(data)
 x$setinwersja(m_1)
 m_1
 # Return a matrix that is the inverse of 'x'
}
