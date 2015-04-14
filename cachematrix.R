## Put comments here that give an overall description of what your
## functions do

##Usage sample
## original_matrix <- rbind(c(1, -1/4), c(-1/4, 1))
## new_matrix <- makeCacheMatrix(original_matrix)
## cacheSolve(new_matrix)
##end of usage sample

## Write a short comment describing this function
##This method create a wrapper variable around input matrix which will cache the result of solve() call
makeCacheMatrix <- function(x = matrix()) {
	#variable to store the inversed matrix
	inverse <- NULL
	
	#set the original matrix
	set <- function(y){
		#using <<- to access the variable not in same environment ( it is the closure variable )
		x <<- y
		inverse <<- NULL
	}
	
	#get original matrix
	get <- function() x
	
	#set the inverse matrix of original matrix
	setinverse <- function(inverse) inverse <<- inverse
	
	#return the inverse matrix of original matrix
	getinverse <- function() inverse
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##This is the real method to get the inverse matrix
##input parameter is the return value for makeCacheMatrix call
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		#try to get the cached result
		inverse <- x$getinverse()
		#if cached result is not null, return the cached result directly
		if (!is.null(inverse)){
			message("Gettting cached inverse matrix")
			return (inverse)
		}
		
		#calculate the inverse matrix and setback to cached item
		originalMatrix <- x$get()
		message(class(originalMatrix))
		inverse <- solve(originalMatrix)
		x$setinverse(inverse)
		inverse
}
