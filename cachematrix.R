##Peer-graded Assignment: Programming Assignment 2: Lexical Scoping


## makeCacheMatrix is a function that creates a special matrix "object" (x).
## It returns a list of functions that stores and get the given matrix and its
## inverse. The list contains:
## [1] Set the value of the matrix (set_func)
## [2] Get the value of the matrix (get_func)
## [3] Set the value of the inverse (set_inv)
## [4] Get the value of the inverse (get_inv)


makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
  set_func <- function(y){
      x<<-y
      i<<-NULL
  }
  get_func <- function() x
  set_inv <- function(inverse) i <<- inverse
  get_inv <- function() i
  list(set_func=set_func,get_func=get_func, set_inv= set_inv, get_inv=get_inv)
}


## cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. Initially, it will check if the inverse has already
## been calculated, this is done using the "if" condition which check if there's
## a value already stored in get_inverse. If there has already values stored, it 
## will skip the computation and retrieve the stored data. Otherwise, it will compute 
## the inverse of the matrix using "solve" function. This will be stored in
## variable "i" and will be stored in makecacheMatrix set_inv as "i" in different environment.
## Finally, the inverse matrix is returned using "i".

cacheSolve <- function(x, ...) {
        i <- x$get_inv()
        if(!is.null(i)){
          message("getting cached data")
          return(i)
        }
        dat <-x$get_func()
        i <- solve(dat)
        x$set_inv(i)
        i
}
