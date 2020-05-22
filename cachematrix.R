## The script uses lexical scoping rules in R to calculate and store the inverse
## of a matrix in cache. This optimises the use of memory.

## makeCacheMatrix function takes the input, i.e. a matrix, from the user 
## for which they wish to get an inverse. The function gives output in the 
##form of a list containing 4 functions that set and get matrix as well as its
## inverse. The variables x and m are initialised. If there is already any value
## stored in the cache for the variables, they are reset to default values, i.e.
## a null matrix for x and null for m. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## This takes the list output from the previous function as input among other 
##inputs that the user might want to give. It gets the value of m, i.e. is 
## the value of inverse matrix from the cache. If the value is already in the
## cache, it returns the same along with the message getting cached data.
## If the value is not found, then it calculates the inverse based 
##on the input from makeCacheMatrix and assigns it to m and returns the same.

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
