## Below function are used to input and cache matrix with its inverse, this is pretty
## simillar what has been explained, in the example of Assignment2. 

## This function inputs a matix and retuns a list with 4 element.
## Elements are simply setting the value of matrix, geting the value of matrix,  
## setting of matrix Inverse, getting  of matrix Inverse   

makeCacheMatrix<- function(x=matrix())
{
  Inv <- NULL
  set <- function(y=matrix()) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setsol <- function(sol=matrix()) Inv <<- sol
  getsol <- function() Inv
  list(set= set, get = get, setsol=setsol,getsol=getsol)
}


## This function looks for the already calculated Inverse of matrix, and return if it
## found. Else, It calculates the Inverse and 
## sets the Inverse using setsol function.

cacheSolve<- function(x,...){
  set<- x$getsol()
  if(!is.null(set)){
    return(set)
  }
  mat<-x$get()
  Inv<-solve(mat,...)
  x$setsol(Inv)
  Inv
}
## Return a matrix that is the inverse of 'x'
