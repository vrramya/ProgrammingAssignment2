#Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
#makeCacheMatrix() and cacheSolve() functions below are used to write the cache of the inverse of the matrix

#The function makeCacheMatrix creates a matrix which is a list containing a function to 
#1 set the value of the matrix
#2 get the value of the matrix
#3 set the value of the inverse of the matrix
#4 get the value of the inverse of the matrix


makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setmatrix<-function(solve)m<<-solve
  getmatrix<-function()m
  list(set=set,get=get,
      setmatrix=setmatrix
      getmatrix=getmatrix)
}

#The function cacheSolve() returns the inverse of the matrix. It initailly checks if the inverse has already been computed. If then, it gets the result and skips the computation. If not, then computation of inverse is performed and sets the value in the cache via setmatrix function. Assumption: that the matrix is always invertible
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