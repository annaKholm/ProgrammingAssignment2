# Matrix inversion is a time-consuming task. Sometimes this task appears several times in the
# given process. In order to avoid repeated computations the cache version of the original matrix as well as its inversion are stored 
# in the cache. 

# In this case each time when in the course of the process there a need to carry out matrix inversion 
# the program checks and compares the matrix to be inversed with the one stored in the cache.
# If the are identical, then instead of inversing the matrix once more it retrieves the inversed matrix from the cache.   

# makeCacheMatrix takes a matrix and saves it in the private variable x
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

# cachemean takes a cached matrix created with makeCacheMatrix
# if we've already computed the inversed matrix and stored it via makeCacheMatrix(),
# and have not invalidated the cache by calling set(), return the cached
# version of x  
# else -  set the inverse in x so we cache it and would not need to recompute it in the future
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



