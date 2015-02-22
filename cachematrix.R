## this function is a list containing 4 functions 
## that allows cacheSolve to call


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  
        set <- function(y) {  
                x <<- y
                m <<- NULL  
        }
        get <- function() x 
        setValue <- function(value) m <<- value 
        getValue <- function() m 
        list(set = set, get = get, 
             setValue = setValue,
             getValue = getValue)
}

## This function takes a matrix, checks if the inverse value has been computed already
## if so, it returns the msg "getting cached data" and grab the cached data
## else it used the created function from the makeCacheMatrix function to calculate and 
## return the value

cacheSolve <- function(x, ...) {
        m <- x$getValue() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() #grab data of x
        m <- solve(data, ...) #perform calculation
        x$setValue(m) #store the calculated value inside the m of makeVector function
        m #return the calculated value
}