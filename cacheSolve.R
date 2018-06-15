cacheSolve <- function(x, ...) {

        # Function intends to take a argument 'x', output from function
        # 'makeCacheMatrix( )' and solve the inverse of that matrix.

        # cachedData <- makecacheMatrix(testMatrix)
        # cacheSolve(cachedData)

        # Expected inverse of 'x' output from cacheSolve:
        # [,1] [,2]
        # [1,] -2 1.5
        # [2,] 1 -0.5

        # In parent environment set variable 'm' to stored function $getinverse

        m <- x$getinverse()

        # If in parent environmnet 'm' is not null, display message on screen
        # and return cached value 'm'.

        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }

        # In parent environment set variable 'data' to stored function
        # $get, the stored matrix.

        data <- x$get()

        # In parent environment set variable 'm' to the function solve() with
        # the variable 'data' as argument.

        m <- solve(data)

        # In parent environmet call listed function 'setinverse'.

        x$setinverse(m)

        # Return 'm'

        m
}
