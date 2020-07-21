## First function calls a matrix which can be inverted and cached

## x is a square invertable matrix. it is followed by a list of functions to set and get the matrix and its inverted table.
## when it has not been calculated the output will display "NULL". When it has, the output will be cached so it can be accessed by the next step. 
makeCacheMatrix <- function(x = matrix()) {
     inv<- NULL
     set<- function(y){
          x<<-y
          inv<<-NULL
     }
     get<- function(){x}
     setInverse<-function(inverse){inv<<-inverse}
     getInverse<- function() inv
     list(set = set, get = get, setInverse= setInverse, getInverse= getInverse)

}
## this function checks the cache of the first. If it finds the inverse is not NULL, it returns the inverse. 

cacheSolve <- function(x, ...) {
        
     inv<- x$getInverse()
     if(!is.null(inv)){
          return(inv)
     }
## If the inverse has not been calculated, it finds NULL and calculates the inverse, sets it in the cache, and returns it.
     mat <- x$get()
     inv<- solve(mat, ...)
     x$setInverse(inv)
     return(inv)
}
