makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set
       , get = get
       , setinv = setinv
       , getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'	        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

x <- makeCacheMatrix(matrix(c(1:4), 2, 2))

summary(x)
x$get()
x$getinv()

cacheSolve(x)
x$getinv()


###############################################
# result
###############################################


#> makeCacheMatrix <- function(x = matrix()) {
#+   inv <- NULL
#+   set <- function(y) {
#+     x <<- y
#+     inv <<- NULL
#+   }
#+   get <- function() x
#+   setinv <- function(inverse) inv <<- inverse
#+   getinv <- function() inv
#+   list(set = set
#+        , get = get
#+        , setinv = setinv
#+        , getinv = getinv)
#+ }
#> cacheSolve <- function(x, ...) {
#+   ## Return a matrix that is the inverse of 'x'	        ## Return a matrix that is the inverse of 'x'
#+   inv <- x$getinv()
#+   if(!is.null(inv)) {
#+     message("getting cached result")
#+     return(inv)
#+   }
#+   data <- x$get()
#+   inv <- solve(data, ...)
#+   x$setinv(inv)
#+   inv
#+ }
#> x <- makeCacheMatrix(matrix(c(1:4), 2, 2))
#> summary(x)
#       Length Class  Mode    
#set    1      -none- function
#get    1      -none- function
#setinv 1      -none- function
#getinv 1      -none- function
#> x$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> x$getinv()
#NULL
#> cacheSolve(x)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> x$getinv()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 