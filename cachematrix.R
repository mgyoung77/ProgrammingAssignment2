## This function will determin the inverse of a matrix
## if the matrix was previously solved, itthe function will
## will recall the previous answer to improve processing time

## creates a list set of functions for setting, assigning,
## solving and recalling the matrix and inverse

makeCacheMatrix <- function(mat = matrix(ncol = 2)){
  inv <- NULL #initizes inverse of matrix as null
  set <- function(m) { #sets matrix and inverse
    mat <<- m #mat gets assigned whatever the m is
    inv <<- NULL #sets the inverse to NULL since it assumes we are reseting
  }
  get <- function() mat #gets the matrix
  getInv <- function () inv #gets the inverse of matrix
  setInv <- function(x) inv <<- x #sets the inverse of matrix with 'x'
  list (set = set, get = get,
        getInv = getInv,
        setInv = setInv) #returns a list of the functions within the parent function for use later
}


## this function calls the list function of makeCacheMatrix with an assigned matrix
## and will determine if the inverse has already been solved and cached

cacheSolve <- function(matri, ...) {
  inver <- matri$getInv() #gets the invserse, the first time it will be NULL
  if(!is.null(inver)) { #if the inverse of matrix is not null (meaning it was cached)
    message("getting cached matrix data")
    return(inver) #returns the inverse without solving it since already stored
  }
  d <- matri$get() #gets the matrix data
  inver <- solve(d,...) #solves the matrix information
  matri$setInv(inver) #sets the inv to the 'inv' directly above
  inver
}
