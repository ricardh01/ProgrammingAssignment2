#This assumes a square matrix is invertible
#to begin, need to pass a value for square matrix x

makeCacheMatrix <- function(x = matrix()) {

  inv_m <- NULL #initialises the inversed cache matrix as NULL, in case this isn't first time used
  
  set <- function(y) { #purpose of this function is to store a new matrix that can be stored in the cache
    x <<- y #new matrix y becomes matrix x to be cached 
    inv_m <<- NULL #erase inversed cache matrix as NULL for previous usage of this function
  }
  
  get <- function() { #returns matrix x in CacheMatrix
    x
  }
  
  setinverse <- function(inverse_matrix) { #stores inverse matrix into cache
    inv_m <<- inverse_matrix
  }
  
  getinverse <- function() { #returns inverse matrix
    inv_m
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
   
    
  inv_m <- x$getinverse() #gets inverse matrix from makeCacheMatrix
    
  if(!is.null(inv_m)) { #if inverse matrix calculated already, return inverse matrix immediately
      
      message("getting cached data")
      
      return(inv_m)
  }
    
  data <- x$get() #if not calculated, pass matrix x as data object
    
  inv_m <- solve(data, ...) #solve data to find inverse of matrix
    
  x$setinverse(inv_m) #store the inverse under the inverse object
    
  inv_m #return the inverse matrix
    
}
