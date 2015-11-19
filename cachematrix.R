#makeCacheMatrix takes a matrix and saves in the private variable x
# matr - a cached matrix
#get - get function for the matrix
#set - set function, reset matrix matr to NULL
#setsolve - set the value of inversed matrix x (Called by cacheSolve)
#getsolve - get the value of the solved matrix.
makeCacheMatrix <- function(x = matrix()) {
    matr <- NULL
    set <- function(y) {
        x <<- y
        matr <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) matr <<- solve
    getsolve <- function() matr
    list(set = set, get = get,
         setsolve = setsolve, 
         getsolve = getsolve)
   }



# cachemean takes a caching matrix created with makeCacheMatrix
# if we've already computed the inverse and stored it via makeCacheMatrix(),
# and have not invalidated the cache by calling set(), return the cached
# version of x  
# else -  set the inverse in x so we cache it and dont need to needlessly
# recompute it
#  x$setsolve(matr) returns the caching matrix matr
cacheSolve <- function(x, ...) {
    matr <- x$getsolve()
    if(!is.null(matr)) {
        message("getting cached matrix")
        return(matr)
    }
    data <- x$get()
    matr <- solve(data)
    x$setsolve(matr)
    matr
}



