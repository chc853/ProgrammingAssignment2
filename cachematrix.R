# Overall, makeCacheMatrix() sustains cache data for resuing it.
# cacheSolve() cacluates the inverse of a Matrix from Matrix or makeCachematrix().
# to validate my won code, you can use the following seqeunces:
# > m <- makeCacheMatrix()
# > m$set(matrix(c(5,3,3,5),2,2))
# > m$get()
#        [,1] [,2]
# [1,]    5    3
# [2,]    3    5
#
# > cacheSolve(m)
#         [,1]    [,2]
# [1,]  0.3125 -0.1875
# [2,] -0.1875  0.3125
#
# > cacheSolve(m)
# getting cached data
#         [,1]    [,2]
# [1,]  0.3125 -0.1875
# [2,] -0.1875  0.3125


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize m
    m <- NULL                                      
    ## Create a function which is to keep global_x and global_m
    set <- function(y) {
        global_x <<- y 
        global_m <<- NULL                                
    }
    
    # Create one line function().
    get <- function() return(global_x)
    set_global_m <- function(m) global_m <<- m    
    get_global_m <- function() return(global_m)                       
    list(set = set, get = get,
         set_global_m = set_global_m,
         get_global_m = get_global_m)
}

# This function computes the inverse of matrix.
# by checking previous history, this function avoids for redundancy.
cacheSolve <- function(x) {
    # try to get the value from the global environment.
    m<- x$get_global_m()               
    if(!is.null(m)) { 
    # by checking if m is NULL, we can know whether this matrix was already computed or not.
    # if so, return computed value in last time, then print the message.
        message("getting cached data")
        return(m)
    }
    # if m is NULL, the inverse of matrix is computed by solve() function.
    # Then, this result should be stored in global value for reusing.
    data <- x$get()               
    inverseMatrix <- solve(data)   
    x$set_global_m(inverseMatrix)             
    return(inverseMatrix)                            
}
