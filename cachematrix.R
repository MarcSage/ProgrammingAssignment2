# usage:  first use makeCacheMatrix to create the Matrix "object"
#         example:  m1<-makeCacheMatrix(matrix(c(1,4,5,4),2,2))
#         now call cachSolve(m1) to set the inverse in the cache
#         finally call cachSolve(m1) again and you'll see
#         that "getting cached data" will print since the inverse
#         is returned without the need to solve() the matrix again

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # set function used update the matrix, resulting
  # in the inverse matrix (i) needing to be "uncached"
  # hence i set to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get simply returns the current matrix
  get <- function() x
  # cache the inverse using setinv()
  setinv <- function(inv) i <<- inv
  # get inverse using getinv()
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  i <- x$getinv()
  # check if returned matrix is NULL, if not, its already been
  # cached so return the cached value
  if(!is.null(i)) {
    message("getting cached data")
    # leave function without need to solve() again
    return(i)
  }
  # get current matrix
  data <- x$get()
  i <- solve(data, ...)  # solve() is R function to calc inverse
  x$setinv(i)
  i
}
