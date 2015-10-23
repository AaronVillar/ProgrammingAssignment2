## Put comments here that give an overall description of what your
## functions do

## Autor Aaron Israel Villar Mata
## aaron.villar@inegi.org.mx

## This functions compute and caches the inverse of the special "matrix"
## To create the cache, it create a special matrix, which has the caching vars

## Write a short comment describing this function

## this function uses DIGEST to calculate if the matriz has change or not, 
## and then calculate or not the solve again

install.packages("digest")
library(digest)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  m5<- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  setmd5 <- function(m_md5) m5 <<- m_md5
  getsolve <- function() s
  getmd5 <- function() m5
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve,
       setmd5 = setmd5,
       getmd5 = getmd5)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  m5 <- x$getmd5()
  vec <- as.vector(x$get())
  hmd5 <- digest(vec,algo="md5")
  # print(hmd5)
  # print(m5)
  if(!is.null(m)) {
    if (m5 == hmd5) {
      message("getting cached data")
      return(m)
    }else{
      message("data has change")
      data <- x$get()
      nr<-nrow(data)
      nc<-ncol(data)
      if (nr==nc){
        m <-  solve(data)
        x$setsolve(m)
        x$setmd5(hmd5)
        return(m)   
      }else{
        print("Not a square matrix")
        return(NULL)
      }
    }
    
  }
  data <- x$get()
  nr<-nrow(data)
  nc<-ncol(data)
  if (nr==nc){
    m <-  solve(data)
    x$setsolve(m)
    x$setmd5(hmd5)
    return(m)  
  }else{
    print("Not a square matrix")
    return(NULL)
  }
}
