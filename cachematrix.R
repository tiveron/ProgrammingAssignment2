## Create a cacheable inverse matrix
makeCacheMatrix <- function( x = matrix() ) {

	## initialize the property
    i <- NULL

    ## get the matrix
    get <- function() {
    	x ## Return matrix
    }

    ## set the matrix
    set <- function( m ) {
            x <<- m
            i <<- NULL
    }

    ## Method to get the inverse of the matrix
    getInv <- function() {
        ## Return the inverse property
        i
    }

    ## set inverse matrix
    setInv <- function( tmp ) {
        i <<- tmp
    }

    ## return a list methods
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Get the inverse matrix, and use cache if it has been already calculated
cacheSolve <- function( x, ... ) {

    ## Return inverse of x
    m <- x$getInv()

    ## return the inverse if its already set
    if( !is.null( m ) ) {
            message( "cached value" )
            return(m)
    }

    ## Get matrix
    data <- x$get()

    ## Calculate the inverse
    m <- solve( data ) %*% data

    ## Set inverse
    x$setInv( m )

    ## Return matrix
    m
}
