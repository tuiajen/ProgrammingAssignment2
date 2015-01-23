##Creating two functions with the purpose of caching a matrix then 
##calling that matrix from a second function to calculate the inverse. 


##The first function allows this function to be assigned to a variable (x in this case).   
##Then 'setting' x to whatever matrix we would like to know the inverse of
##so that it can be called over and over via the stored in memory get function. It creates 4
##functions: Ses the value of matrix, gets the value of a matrix, sets the inverse of the
##matrix then gets then inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The second function allows passing of a cached matrix from the first function, ensuring it 
##hasn't already been calculated then 
##produces the inverse of the matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
              message("getting cached data")
              return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m    
}
