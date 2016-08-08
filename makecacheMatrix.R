makecacheMatrix <- function(x = matrix()) {
        
        ## Example matrix 'x' supplied to makecacheMatrix(x):
        ## [,1] [,2]
        ## [1,] 1 3
        ## [2,] 2 4
        
        ## cachedData <- makecacheMatrix(testMatrix)
        ## Pass 'cachedData' to cacheSolve(cachedData)
        
        ## Set variable 'm' representing matrix inverse to NULL in parent
        ## environment.
        
        m <- NULL
        
        ## In parent environment set variable 'set' to a function, define that
        ## function's current environment to 1) replace 'x' with 'y' in the
        ## main function makecacheMatrix parent environment, and 2) sets
        ## variable 'm' to NULL in parent environment.
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## In parent environment set variable 'get' to a function, defining that
        ## function to return 'x'.
        
        get <- function() x
        
        ## In parent environment set variable 'setinverse' to the function
        ## 'solve' and set variable 'm' from current environment to 'solve' in
        ## parent environment.
        
        setinverse <- function(solve) m <<- solve
        
        ## In parent environment set variable 'getinverse' to function that
        ## returns value of 'm'.
        
        getinverse <- function() m
        
        ## Listify all parent environment variables established above.
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}
