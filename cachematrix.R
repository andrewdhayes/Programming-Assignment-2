## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is a function that creates a special "matrix" object that 
# can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initializes the object x as a function argument, and the object inv to be used
  # later on in the function
  inv <- NULL
  
  # makes the set() function which assigns the input variable to the x object in
  # the parent environment, and clears any value of inverse from previously
  # running the cacheSolve function
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # retrieves the value of x from the parent environment
  get <- function() x
  
  # a function to find the inverse of the matrix
  setinverse <- function(solve) inv <<- solve
  # a function to return the inverse of the matrix
  getinverse <- function() inv
  
  # names the list elements which allows us to access the functions later 
  # using the '$' operator
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


# cacheSolve is a function that computes the inverse of the special "matrix" 
# returned by makeCacheMatrix, and retrieves the inverse from the cache if it 
# has already been calculated 
cacheSolve <- function(x, ...) {
  
  # retrieves the inverse  
  inv <- x$getinverse()
  # if the inverse has already been computed by a previous run of the cacheSolve
  # function (if it is not NULL), then this will return the inverse that is cached,
  # without going to the extra effort of re-calculating it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if the inverse has not been computed yet, it will retrieve the new object 
  # entered and run the solve function to find the inverse and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
