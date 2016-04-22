## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv<-NULL ##sets default value for inverse
  set<-function(y){
    xinv<<-NULL
    x<<-y
  }
  get<-function() {x}
  setInverse<-function(inver){xinv<<-inver}
  getInverse<-function() {xinv}
  isInverted<-function() {FALSE}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  xinv <- x$getInverse()
  data <- x$get()
  
  ##check if oldx matrix exists. It is created the first time you run call the function
  if(!is.null(r <- get0("oldx", envir = environment()))){
    ##checks if the input matrix has changed or not
    if( isTRUE(all.equal(oldx,data))) {
      print("getting cached data")
      ##return(oldxinv)
      return(oldxinv)
    }
    
  }
  ##the actual matrix inversion!!
  xinv <- solve(data)
  x$setInverse(xinv)
  ##keeps the old INPUT matrix for checking next time the function is called
  oldx<<-x$get()
  ##keeps the inverse matrix cached
  oldxinv<<-xinv
  xinv
}

