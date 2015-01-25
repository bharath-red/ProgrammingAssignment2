## Finding the inverse of a matrix can be a potentially time consuming 
## operation. The below functions calculate the inverse of a matrix, and cache
## the resultant inverse matrix. If the inverse needs to be used repeatedly, 
## the inverse would be retrieved from the cache, saving precious computation.

## function: makeCacheMatrix.
## This function creates an object which intializes the original matrix and 
## defines APIs which can be used to set and get the original matrix and its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initaializing a NULL matrix which would be returned if the inverse 
    ## matrix has not yet been computed
    dimMat <- dim(x)
    invMat <- matrix(0, dimMat[1], dimMat[2])

    ## Initializing APIs
    setMat <- function(mat1) {
        x <<- mat1
        dimMat <- dim(x)
        invMat <- matrix(0, dimMat[1], dimMat[2])
    }   
    getMat <- function() x
    setInv <- function(inv) invMat <<- inv
    getInv <- function() invMat
    list(setMat = setMat, getMat = getMat,
            setInv = setInv,
            getInv = getInv)

}


## function: cacheSolve
## This function on its intial invocation computes the inverse matrix and 
## caches it. Upon subsequent invocations the cached inverse matrix is returned.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    invMat <- x$getInv()
    mat <- x$getMat()

    ## Initialize an identity matrix of the dimensions of original matrix
    idMat <- diag(nrow(mat))

    ## Check if inverse matrix has been computed 
    if(identical(idMat,(mat %*% invMat)))
    {
        message("Getting cached Inverse")
        return(invMat)
    }

    ## If inverse matrix not computed, compute it and cache the result
    mat <- x$getMat()
    invMat <- solve(mat, ...)
    x$setInv(invMat)
    invMat
}
