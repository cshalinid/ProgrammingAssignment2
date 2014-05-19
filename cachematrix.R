## The functions here uses the passed Matrix/data x 
## to computes its inverse 
## and caches the inverse to avoid recomputation 
##
## Example:
##  source("cacheMatrix.R")
##  a<-replicate(4, rnorm(4))
##  f<-makeCacheMatrix(a)
##  f<-makeCacheMatrix(a)
##  cacheSolve(f) # computes inverse for first time and caches.
##  cacheSolve(f) # returns the cached value



## makeCacheMatrix will compute the inverse of the matrix x
## inverse of x is stored in special object m

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
              x<<-y
              m<<-NULL
        }
        get<-function() x
        setMatrix <- function(mat) m<<-mat
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## Write a short comment describing this function

## cacheSolve will return either the chached inverse of matrix  x
## Otherwise calls makeCacheMatrix to compute the inverse of given matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrix(m)
        m
}
