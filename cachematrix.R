## This program contains code to create a matrix stored in an a separate environment 
which can hold the inverse of the matrix x.
makeCacheMatrix <- function(x) {
		abe<-nrow(x)
		m<-matrix(nrow=abe,ncol=abe)
        set <- function(y) {
                x <<- y
                m<-matrix(nrow=abe,ncol=abe)
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
             }


## This program tests for the presence of in a separate environment of the inverse
of a matrix x. When the inverse has been previously calculated, the inverse matrix
is retrieved from the separate environment. When it has not been previously determined,
the program instructs the solution of the inverse using the function solve.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        are<-m[1,1]
        if(!is.na(are)) {
                message("getting cached data")
                return(m)
        }
        else{
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m}
}