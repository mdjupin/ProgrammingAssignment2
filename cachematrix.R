# Hear are two functions: 
# makeCacheMatrix and cacheSolve.
# -------------------------------------------------------------
# makeCacheMatrix is independent of cacheSolve, but cacheSolve
# is dependent on makeCacheMatrix. Together tthis pair of 
# function help to store a square matrix (mat) and calculate 
# its inverse (invmat).
# makeCacheMatrix has as argument a square matrix (mat).
# cacheSolve has as argument makeCacheMatrix or and object that
# was assign makeCacheMatrix (e.g. work <- makeCacheMatrix(mat) ).
# For the stored variable (mat and invmat) from makeCacheMatrix 
# to be available to # cacheSolve, the operator '<<-' is used to 
# store the variable (mat and invmat) in the parent environment.
# -------------------------------------------------------------

# -----------------------START---------------------------------
# 1. makeCacheMatrix, it is used to store a matrix (mat)
# and it's inverse matrix (invmat). When a new matrix is 
# feeded, the invmat is initiaziled to NULL. However, be careful
# when a new invmat is feeded, mat will not be initialized. 
# In side makeCacheMatrix, ther are 4 functions (setmat, 
# setinvmat, getmat, getinvmat) and 2 objects (mat, invmat).

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  
  # 1.1. setmat() is used to create the mat objet and depend on 
  # the argument that is a sqaure matrix. It also initialize
  # the invmat object that is the inverse of the matrix mat.
  setmat <- function(feed_mat) {
    x <<- feed_mat
    invmat <<- NULL
  }
  
  # 1.2. setinvmat() is used to create the invmat objet
  # independently of the mat object.
  getmat <- function(){
    x
  } 
  
  # 1.3. getmat() return the mat object.
  setinvmat <- function(feed_inv_mat){
    invmat <<- feed_inv_mat
  } 
  
  # 1.4. getinvmat() return the invmat object.
  getinvmat <- function() invmat
  
  # 1.5 Liste of embeded functions.
  list(setmat = setmat, getmat = getmat,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

# -------------------------------------------------------------
# 2. cacheSolve, it is used to find the inverse matrix (invmat)
# when is saved by the makeCacheMatrix, if the inverse
# matrix is absent, it will calculate the inverse matrix (invmat)
# from the matrix (mat) provided by the makeCacheMatrix.
# There are two section in the cacheSolve function: 

cacheSolve <- function(x, ...) {
  retrieve_invmat <- x$getinvmat()
  
  # 2.1. first to check if the invmat is the present and return it; 
  if(!is.null(retrieve_invmat)) {
    message("getting cached data")
    return(retrieve_invmat)
  }
  
  # 2.2. second if invmat is NULL, to calculate invmat from mat 
  # using the solve() function.
  data <- x$getmat()
  calculate_invmat <- solve(data, ...)
  x$setinvmat(calculate_invmat)
  calculate_invmat
}
# ------------------------END----------------------------------