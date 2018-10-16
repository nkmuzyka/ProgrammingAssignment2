











makeCacheMatrix <- function(mx = matrix()) {

	imx <- NULL

	

	get <- function() return(mx)

	

	getInverse <- function() return(imx)

	

	set <- function(val) {

		mx <<- val

		inverse <<- NULL

	}

	

	setInverse <- function(val) imx <<- val

	

	return(

		list(

			set = set,

			get = get,

			setInverse = setInverse,

			getInverse = getInverse

		)	

	)

}















cacheSolve <- function(cmx, ...) {

	imx <- cmx$getInverse()

	

	if (!is.null(imx)) return(imx)

	

	mx <- cmx$get()

	imx <- solve(mx, ...)

	cmx$setInverse(imx)

	

	return(imx)

}
