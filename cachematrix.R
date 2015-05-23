## Function for storing the matrix function
## Made this list of functions to create the 
## caching capability required for the assignment

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL  ##Wipes the slate clean if needed
  load<- function()x  ##sets up a function that can provide the orginial matrix
  cacheinv<-function(inv)inverse<<- inv  ##This is the function that provides the caching capability
  loadinverse<-function()inverse  ##sets up a function that provides the current value of the inverse. Mainly used to check to see if there is a value cached.
  reset<- function(z) {##Lets you change the matrix values in the special "matrix" created by makeCacheMatrix. Not required but it was in the example and useful for testing the program.
    x<<-z           ##This line ensures the object x will now be the new value in all environments
    inverse<<-NULL  ##This line eliminates any previously cached matrix since the value of x has changed
  }
  ##Stores the functions in a list so they can be called with x$
  list(load = load, cacheinv = cacheinv, loadinverse = loadinverse, reset = reset)
}


##Write a short comment describing this function
##This function provides the inverse of the special "matrix" created by the makeCacheMatrix function
##If the inverse has already been calculated it returns it immediately
cacheSolve <- function(x, ...) {
  inverse<- x$loadinverse() ##This checks to see if there is an inverse value already saved
  if(!is.null(inverse)){  ##If there is a value cacheSolve immediately provides it
    return(inverse)
  }
  matrixdata<- x$load() ##Otherwise an object is created with the matrix values
  inverse<- solve(matrixdata, ...) ##Solve function is used to calculate the inverse
  x$cacheinv(inverse)  ##The newly calculated value is cached so it can be called upon again.
  return(inverse)  ##The value of the inverse is provided
  
  ## Return a matrix that is the inverse of 'x'
}