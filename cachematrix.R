## Function stores a matrix with makeCacheMatrix()
## ready to be 'used' by cacheSolve() function.
## cacheSolve() returns the inverse of the matrix stored in
## makeCacheMatrix()

## makeCacheMatrix():
## Sets and gets of matrix input in m variable.
## Sets and gets solve() in s variable.


makeCacheMatrix <- function(m = matrix()) {
	s <- NULL
	set <- function(y) {
		m <<- y
		s <<- NULL
	}
	get <- function() m
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## cacheSolve():
## Function returns a inverse of the matrix provided
## with solve()

# Function first trys to get getsolve() and store in "s",
# if result of "s" is NOT null; return a message and stored "s".

# Otherwise get matrix with "m$get()" and store in "data".
# Call function solve(), passing "data" and store in "s";
# set s to global with m$setsolve(s)
# finally returns "s"

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- m$getsolve()
        if(!is.null(s)) {
        	message("getting cached data")
        	return(s)
        }
        data <- m$get()
        s <- solve(data, ...)
        m$setsolve(s)
        s
}
