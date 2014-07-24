## These two functions combined save computational time by preventing the repeated calculation of inverse matrices. Before calculating a fresh inversion, the functions check whether previous inversions of the matrix have been stored, and if so retrieve them from storage.


## This function, makeCacheMatrix, caches matrices and there inverse partners.

makeCacheMatrix <- function(a) 
{
b<-matrix(a,sqrt(length(a)),sqrt(length(a)))
	    m<-NULL
        set <- function(y) {
                b <<- y
                m<<-NULL
        }
        b<-matrix(a,sqrt(length(a)),sqrt(length(a)))
        get <- function() b
        setmatrix <- function (solve) m <<- setmatrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
             
}


## This function checks for previous inversions of a matrix. If it does not find any it creates a new inversion and stores it.

cacheSolve <- function(a, ...) 
{
        m <- a$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- a$get()
        m <- solve (data, ...)
        a$setmatrix(m)
        m
}