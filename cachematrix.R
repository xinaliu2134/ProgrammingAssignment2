## The first function, makeCacheMatrix create a special matrix, which is a list contains a function to
## 1. initialize the cache Matrix 'cacheMatrix'
## 2. assign the value NULL for the first initialization
## 3. define the method named 'setMatrix'
## 4. define the method named 'getMatrix', return the matrix 'x'
## 5. define the method named 'setCache'
## 6. define the method named 'getCache', that will return the cached inverse of 'x'
## 7.  list the names of all methods that will be known to the outside world

makeCacheMatrix <- function(x = matrix()) {
        cacheMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        getMatrix <- function() x
        setCache <- function(inverse) cacheMatrix <<- inverse
        getCache <- function() cacheMatrix
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setCache = setCache,
             getCache = getCache)
}


## 'cacheSolve' return the inverse of a given matrix utilizing the cache
## check the content of cache matrix
##if the content is not null then: return the result
##if the content is empty then: get the matrix, create, set, update and return the cache matrix


cacheSolve <- function(x, ...) {
        cacheMatrix <- x$getCache()
        if (!is.null(cacheMatrix)) {
                message("loading cache matrix...")
                return(cacheMatrix)
        }
        else {
                dMatrix <- x$getMatrix()
                cacheMatrix <- solve(dMatrix, ...)
                x$setCache(cacheMatrix)
                return(cacheMatrix)
        }
}
