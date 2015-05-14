## Assignment 2
## Code based on provided example: Caching the Mean of a Vector, 
## adapted so it would instead cache a Matrix

## These two functions work together in order to create a special object
## that exploits the scoping rules of R in order to cache it's inverse so
## that the inverse does not have to be re-computed. These functions assume
## that matrices provided are able to be inversed.

## The user of these functions first needs to convert thier matrix into the 
## matrix object by calling the makeCacheMatrix function on thier matrix.
## Then, the user can compute the inverse of the matrix by calling the
## cacheSolve function on the object returned by the makeCacheMatrix call.
## The cacheSolve function will either retrieve the inverse from where it is
## cached or calcualate the inverse if it is not yet cached.

## Creates a special "matrix" object that can cache its inverse
## Param x: the matrix to convert to the special "matrix" object
## Returns: special "matrix" object
makeCacheMatrix <- function( x = matrix() ) {
      ## Create special "matrix" object, a list containing a function to:
            ##    1. set the value of the matrix
            ##    2. get the value of the matrix
            ##    3. set the value of the inverse
            ##    4. get the value of the inverse
      
      ## Default set the inverse to NULL
      inv <- NULL
      
      ## Define the function for setting the value of matrix x
      set <- function( z ){
            x <<- z
            inv <<- NULL
      }
      ## Define the function for getting the value of the matrix
      get <- function(){
            x
      }
      ## Define the function for setting the inverse of the matrix x
      setinverse <- function( inverse ){
            inv <<- inverse
      }
      ## Define the function for getting the inverse of the matrix x
      getinverse <- function(){
            inv
      }
      ## Return a list containing references to the four functions defined above
      list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
      
}

## Computes the inverse of the special "matrix" object returned
## by makeCacheMatrix. If the inverse has already been calculated,
## then cacheSolve should retrieve the inverse from the cache
## Param x: the matrix "object" created by makeCacheMatrix
## Returns: a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      ## Try and get the inverse of x from the special "matrix" object
      inv <- x$getinverse()
      ## If the inverse is not NULL, return the previously calculated inverse
      if( !is.null(inv) ){
            message( "Getting cached data." )
            ## Return inverse and exit function
            return( inv )
      }
      ## If the inverse has not yet been calculated, solve for the inverse, cache it,
      ## and return it
      mat <- x$get()
      inv <- solve( mat, ... )
      x$setinverse( inv )
      inv
}
