# This function is able to cache inverse matrix calculations
# Output of "makeCacheMatrix" is  a list of functions in contines

makeCacheMatrix <- function( x = matrix() ) {
  
  # initialize NULL-matrix
  m = NULL
  
  # set matrix
  set <- function( y ) {
    
    # set input matrix to global x
    x <<- y
    
    # clear matrix if exists
    m <<- NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set value of the inverse of matrix
  setinverse <- function(solve) m <<- solve
  
  # get value of the inversed matrix
  getinverse <- function() m
  
  # List of functions
  list( set = set, 
        get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
  
}

# This function get matrix and wates if its inversion exists in cache, if not then it calculates inversion and sets it to cache (using subfunctions from makeCacheMatrix)

cacheSolve <- function( x, ... ) {
  
  # get value of inversed 
  m <- x$getinverse()
  
  # if inverse is already exists
  if(!is.null(m)) {
    message("getting cached data")
    
    # return cached value
    return(m)
    
  }
  
  # calculates inverse matrix
  str_m <- x$get()
  m <- solve(str_m, ...)
  
  # set result to cached value
  x$setinverse(m)
  
  # return inverse marix
  m
}