## This File contains functions for finding and caching the inverse of a matrix
## It is part of Assignment 2 for R programming course.

## This function returns a 'special matrix' object with the ability to cache its inverse.
## IMPORTANT: do not try to use the returned object directly as a matrix, instead use
## its get method to get its data and set to set them. 
makeCacheMatrix <- function(x = matrix()) {
    cached_inverse <- NULL
    set <- function(m){
        x <<- m
        cached_inverse <<- NULL
    }
    get <- function() x
    cache_inverse <- function(im) cached_inverse <<- im
    get_cached_inverse <- function() cached_inverse
    list(set = set, get = get, cache_inverse = cache_inverse, get_cached_inverse = get_cached_inverse)
}


## This function takes a 'special matrix' as an argument and returns its inverse
## Note though that the returned matrix is not a 'special matrix' but a standard
## r matrix
cacheSolve <- function(x, ...) {
    ci <- x$get_cached_inverse()
    if(!is.null(ci)){
        return(ci)
    }
    else{
        m <- x$get()
        inverse <- solve(m)
        x$cache_inverse(inverse)
        return(inverse)
    }
}
