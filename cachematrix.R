## pbolfing
## makeCacheMatrix stores matrix and inverse for use in cacheSolve
## cacheSolve determines if inverse needs to be calculate or pulled from cache

## The function takes a matrix and caches it and its inverse additionally
## it allows for the retrieval of the cached matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
     
     ## place holder for cached inverse matrix
     cache_inv_mat <- NULL
     
     ## set matrix
     set_mat<- function(y){
          
          ## hold cached matrix
          x  <<- y
          
          ## make cached inverse null if new matrix set
          cache_inv_mat <<- NULL
          
     }
     
     ## get cached matrix
     get_mat <- function () x
     
     ## set cached inverse matrix
     set_inv_mat <- function (inv_mat) cache_inv_mat <<- inv_mat
     
     ## get cached inverse matrix
     get_inv_mat <- function () cache_inv_mat
     
     list(set_mat = set_mat, get_mat = get_mat, 
          set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)

}


## takes a matrix, sees if the inverse has been calculated and matrix unchanged
## and returns inverse matrix, if not calculated or matrix changed calculates 
## inverse

cacheSolve <- function(x, ...) {
     
     ## get cached inverse matrix
     c_inv_mat <- x$get_inv_mat()

     ## get cached matrix
     c_mat <- x$get_mat()
     
     ## see if not null and matrix same
     if(!is.null(c_inv_mat)){
          
          message("getting cached inverse matrix")
          
          return(c_inv_mat)
          
     }## close if
     
     ## calculate inverse of cached matrix
     c_inv_mat <- solve(c_mat)
     
     ## set cached inverse matrix
     x$set_inv_mat(c_inv_mat)
     
     ## return value
     c_inv_mat
     
}
