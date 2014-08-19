## makeCacheMatrix
## Creates a special "matrix" object that can cache its inverse.
## Saves the matrix to variable x and its inverse to variable m.
## set: sets matrix and resets cached inverse
## get: returns matrix
## setmatrix: saves solve value
## getmatrix: returns cached inverse value
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Function to get the inversed matrix from a special object created by makeCacheMatrix.
## Takes the object of that type as an argument 'x', checks if the inverse value is already
## cached, and if it is returns the cached value; if not, this function calculates the
## inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setSolve'
## and returns the result.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}