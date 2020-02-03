
# "R version 3.6.2 (2019-12-12)"

## Program week 3 - Assignment  


## makeCacheMatrix() caches matrix into the superassigned variable 'i'
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # if I set a new matrix the cache is cleared
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() retrieves matrix from cache (if superassigned variable is not null)
## In addition I handled cases of wrong input, not square matrix and zero determinant  
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("INFO: getting cached data")
        return(i)
    }
    
    data = x$get()
    
    if(!is.matrix(data)) {
        message("ERROR: not matrix input")
        return(data)
    }
    if(nrow(data) != ncol(data)) {
        message("ERROR: not a square matrix, hence not invertible")
        return(data)
    }
    
    if(det(data) == 0) {
        message("ERROR: zero determinant, hence matrix not invertible")
        return(data)
    }
    i <- solve(data)
    x$setInverse(i)
    i
}



# ERROR: x not matrix
m1 <- "test"
mc <- makeCacheMatrix(m1)
cacheSolve(mc)

# ERROR: x not squared
m1 <- matrix(c(1/2, -1/4, -1, 3/4, 1, 5), nrow = 2, ncol = 3)
mc <- makeCacheMatrix(m1)
cacheSolve(mc)


# ERROR: zero determinant
m1 <- matrix(c(1, 10, 1, 10), nrow = 2, ncol = 2)
mc <- makeCacheMatrix(m1)
cacheSolve(mc)

# OK matrix invertible
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
mc <- makeCacheMatrix(m1)
cacheSolve(mc)

# get from cache
cacheSolve(mc)

# change matrix
m1 <- matrix(c(2, 5, 9, 0), nrow = 2, ncol = 2)
mc$set(m1)
cacheSolve(mc)

# get from cache
cacheSolve(mc)










