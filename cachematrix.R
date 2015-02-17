## 
## Creates a cache for a matrix and its inverse.
## 
##
##
## Two Functions-:
##      makeCacheMatrix: Basically, lexically scopes a matrix and acts as
##                       a cache by storing both the matrix and its 
##                       inverse. Defines 4 functions (set,get,setinv,getinv)
##                       Returns a list of these 4 functions for accessing &
##                       updating both the matrix and its inverse respectively
##                       by the different functions that are returned by the
##                       list. Since, lexical scoping has been used 
##                       appropriately the values remain within the scope to
##                       create a proper cache of both the matrix and its 
##                       inverse.
##
##      cacheSolve:      First Checks whether the inverse is stored 
##                       in CACHE; which is basically the scope of 
##                       "makeCacheMatrix". If it is there; then returns the
##                       cached value (value stored in the scope of 
##                       "makeCacheMatrix"). Else it calculates the inverse
##                       and then updates the value of the matrix and the value
##                       of the inverse in Cache (scope of "makeCacheMatrix")
##                        
## 

## Creating a cache for a matrix to store both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {  
        ## NULL initilization. Same Condition will be checked in cacheSolve fn.
  inv<- NULL   
  
        ## This is will be used to update the value of the matrix.  
        ## Whenver it gets updated, its inverse is set to NULL. Both 
        ## the matrix and inverse lexically scoped.  
  set<- function(y) {
    x<<- y
    inv<<- NULL
  }
  
        ## Functions to retrieve matrix value, updating inverse value, and 
        ## retrieving inverse values.  
  get<- function() x
  setinv<- function (inverse) inv<<-inverse
  getinv<- function () inv
  
        ## Return from the function that is a list of four functions.  
  list( set= set, get= get, setinv=setinv, getinv=getinv) 
  
}


## Function to access the Cache value of Matrix and return inverse.
## Also updates the Cache if the inverse value in cache is NULL

cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinv()
  if(!is.null(inv)) {
    message ("Getting Cached Data")
    return(inv)
  }
  
  ## Functions to retrieve matrix value, solving inverse and updating 
  ## inverse value in cache.
  data<-x$get()
  inv<- solve(data)
  x$setinv(inv)
  inv
}
