# set() lets us pass a matrix as the input to makeCacheMatrix function.
# get() gives us what matrix has been passed as the input.
# setinverse() takes the inverse of the matrix from cacheSolve() and passes it to makeCacheMatrix()
# getinverse() gives the inverse of the matrix as a result

## makeCacheMatrix() takes a matrix as the input and gets the inverse of the matrix from cacheSolve() and stores it.

makeCacheMatrix <- function(x = matrix()) {
  
  m<-matrix()
  
set<-function(y=matrix()){
  x<<-y
  m<<-matrix()
}

get<-function()x

setinverse<-function(inverse) m<<-inverse
getinverse<-function() m
  
 list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)   
}


# cacheSolve() gets the matrix that is to be inverted from makeCacheMatrix() and calculates the inverse of it if it cannot find an already
#inversed and stored version of the matrix in makeCacheMatrix(). If cacheSolve() finds an already inversed and stored version, it retrieves
#this matrix from makeCacheMatrix().

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
