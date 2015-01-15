
## Returns a list with 4 funcions
## invcache is the variable that contains the inverse of the matrix
## set is a function that allows the user to change the value of x without calling makecachematrix again
## get allows the user to get the underlying vector in order to compute anything he wants
## setinverse assigns the value of the inverse to the invcache variable
## getinverse returns the value of the cached inverse (invcache)

makeCacheMatrix <- function(x = matrix()) {
            invcache<-NULL
            set<- function(y){
                    x<<-y
                    invcache<<-NULL
            }
            get <- function() x
            setinverse <- function(invert) invcache <<- invert
            getinverse <- function() invcache
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Cachesolve first checks whether an inverse for the matrix has been cached.
## If there is a cached version it returns this cached version that is stored in the invcache variable
## If not (i.e. invcache is NULL) then it computes the inverse
## It gets the data for the matrix in question, computes the inverse, caches the inverse for a possible future use and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invcache <- x$getinverse()
        if(!is.null(invcache)) {
                message("getting cached data")
                return(invcache)
        }
        data <- x$get()
        invcache <- solve(data, ...)
        x$setinverse(invcache)
        invcache
}
