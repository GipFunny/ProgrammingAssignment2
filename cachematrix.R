## Second assignment: Lexical scoping
## For increased efficiency, these functions are 
## used to extract an inverse matrix from cache 
## rather than calculation.

##--------------------------------------------------
## Use:
## 1. myCacheMatrix <- makeCacheMatrix()
## 2. myCacheMatrix$set(matrix(rnorm(100), 10, 10))
## 3. mySolvedMatrix <- cacheSolve(myCacheMatrix)

## A second call to line 3 will produce the message: from cache

## To view the data in the original matrix:
## myCacheMatrix$get()

## Create a special matrix that can be cached

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setsolved <- function (solved) m <<- solved
    
    getsolved <- function() m
    
    list(set = set, get = get, setsolved=setsolved, getsolved=getsolved)
}


## Returns the solved matrix from cache, if available,
## or calculation, if not.

cacheSolve <- function(x, ...) {
    m <- x$getsolved()
    
    if (!is.null(m)){
        message("from cache")
        return (m)
    }
    
    data <- x$get()
    
    m <- solve(data)
    
    x$setsolved(m)
    
    m
}
