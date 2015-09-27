makeCacheMatrix <- function(a = matrix()) {
    inv_a <- NULL
    set <- function(b) {
        a <<- b
        inv_a <<- NULL
    }
    get <- function() a
    setinverse<- function(inverse) inv_a <<-inverse
    getinverse <- function() inv_a
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
cacheSolve <- function(a, ...) {
  
    inv_a <- a$getinverse()
    if (!is.null(inv_a)) {
        message("getting cached ")
        return(inv_a)
    } else {
        inv_a <- solve(a$get())
        a$setinverse(inv_a)
        return(inv_a)
    }
}
