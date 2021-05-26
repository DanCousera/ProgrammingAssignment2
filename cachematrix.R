## The following functions are used to cache the inverser of a matrix.
## A special "matrix" object x is introduced which caches its inverse.
## To create a "special" matirx call \code{m <- makeCacheMatrix(x)}
## where x is an ordinary matrix. 
## Its value can be retreived and modified by  \code{m$get()}
## and \code{m$set(y)}. The inverse of the "special" matrix is computed 
## with \code{cacheSolve(m)}. The inverse is only computed if
## it has not already been computed before.

#'makeCacheMatrix
#'
#'Create a special "matrix" object that can cache its inverse.
#' 
#' @param x A matrix
#' 
#' @return A list containing functions to set and get the value of the
#'     matrix and to set and get the inverse of the matrix
#'   

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL # the inverse 
        set <- function(y) {
                if(identical(x,y)){
                        # no need to set the matrix, if it has already been set 
                        # with the same values
                        matix_has_changed <<- FALSE 
                }
                else{
                        x <<- y # init the "special" matirix with given matrix y
                        inverse <<- NULL # set inverted matrix to NULL
                        matix_has_changed<<- TRUE
                }
        }
        
        get <- function() x # return "special" matrix
        setInverse <- function() inverse <<- solve(x) #calculate the inverse
        getInverse <- function() inverse # return inverse
        # return list of functions making them avaiable for cacheSolve function
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

#'cacheSolve
#'
#'Return inverse of matrix x
#' 
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve retrieves the 
#' inverse from the cache.
# 
#' @param x a special matrix created with makeCacheMatrix
#' 
#' @return The inverse of the matrix x
#' 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get cached value
        inverse <- x$getInverse() 
        
        ## compute the inverse if it has not been computed 
        ## before or the matrix or the matrix has changed
        ## store the inverse matrix in cache,
        if (is.null(inverse) || matrix_has_changed) {
                mat <- x$get()
                inverse <- solve(mat, ...)
                x$setInverse(inverse)
        }
        #return inverse of matrix
        inverse
}
