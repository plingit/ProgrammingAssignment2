##  `makeCacheMatrix` function creates a special "vector", which is
##   a list containing a function to
##     1.  set the value of the matrix
##     2.  get the value of the matrix
##     3.  set the value of the inverse of the matrix
##     4.  get the value of the inverse of the matrix


##  << operator is used to assign the objects x (the matrix) and minv (the inverse of the matrix) 
##  in the global environment, so they are "cached".

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y        
        minv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) minv <<- inverse
    getInverse <- function() minv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


##  cacheSolve function calculates the inverse of the special "matrix"
##  created with the above function makeCacheMatrix. It first checks to see if the
##  inverse has already been calculated. If so, it `get`s the inverse(minv) from the
##  cache and skips the computation. Otherwise, it calculates the inverse of
##  the matrix (data) and sets the inverse matrix in the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getInverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
   data <- x$get()
   minv <- solve(data, ...)
   x$setInverse(minv)
   minv  
}

## The following are two examples to use the functions. They are not required to read.
## Example run 1:
# source('cachematrix.R')
# > mat1<-matrix(c(2,2,3,2),2,2)
# > mat1
# [,1] [,2]
# [1,]    2    3
# [2,]    2    2
# > cachem1<-makeCacheMatrix(mat1)
# > cacheSolve(cachem1)
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
# > cacheSolve(cachem1)
# getting cached data
# [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0

## Example run 2:
#> bigmatrix<-matrix( rnorm(1000*1000,mean=0,sd=1), 1000, 1000)
#> cachem2<-makeCacheMatrix(bigmatrix)
#> cacheSolve(cachem2)
## run it again, it will be much quick.
#> cacheSolve(cachem2)