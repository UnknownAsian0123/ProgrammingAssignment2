##finds inverse for square matrix
inverse <- function(x){
  ##non square matrix don't have inverse
  if(nrow(x) != ncol(x))
  {
    return(NULL)
  }
  
  d <- 1
  eValues <- eigen(x)$values
  
  ##finds determinant by using eigen values
  for(i in 1:length(eValues)){
    d <- d * eValues[i]
  }
  
  ##determinant cannot be zero if matrix is invertible
  if(d == 0)
    return(NULL)
  
  ##adjoint function from matlib package
  matrix(as.numeric(adjoint(x)/d), nrow(x), ncol(x)) 
}

##special matrix function that caches inverse
makeCacheMatrix <- function(x = numeric()) {
  invr = NULL
  
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) invr <<- inverse
  
  getInverse <- function() invr
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cache for inverse of matrix
cacheSolve <- function(x){
  invr <- x$getInverse()
  
  if(!is.null(invr)){
    message("getting cached data")
    return(invr)
  }
  
  data <- x$get()
  invr <- x$setInverse(inverse(data))
  invr
}