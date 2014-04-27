## The idea here is to test if inverse of a matrix is in Cache.
## If it is, result is fetched from the cache along with "getting cached data" message and output is displayed
## If not, result is dumped into cache and retrived later.  

## Returns a list  of four items i.e set, get,setsolve(computes inverse), getsolve(fetches inverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## Getting Inverse of a matrix. Checking to see if result is in cache. If it is, "getting cached data" message along with output is show". 
## If not, result is directed to x's cache for retrieval,later.. 

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
