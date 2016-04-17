## Programming Assignmen 2 By Javad Hashtroudian
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix is a function to demonstrate lexical scoping in R
# it accepts a matrix and return a list of 4 functions
# set: sets a matrix
# get: returns the same matrix
# setmatrix produces the inverse
# getmatrix returns the matrix
# under normal circumstances the functions in the returned list are not to be used but are used in cacheSolve
# I did use them during debug
# to use these two functions first run makeCacheMatrix and then pass the returned list to cacheSolve to get the inverse
# the first time cacheSolve uses solve to produce the inverse the second and later times it returns the cahed value
# see test 1 for example od usage
# see test2 for proving that the caching actually works and saves time on large matrices thanks to caching

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(matrix) m <<- matrix
	getmatrix <- function() m
	list(set = set, get = get,
		 setmatrix = setmatrix,
		 getmatrix = getmatrix)
}


## Write a short comment describing this function
# note solve(a, b, tol, LINPACK = FALSE, ...) takes variabe number of 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if(!is.null(m)) {
			message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
		
}

## Write a short comment describing this function
# note solve(a, b, tol, LINPACK = FALSE, ...) takes variabe number of 
# same as cacheSolve to test for timing without message
# (printing to stdout takes time and should not be times in itterations)
cacheSolve1 <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if(!is.null(m)) {
			#message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
		
}


# test1 just tests the system and demonstrates that it works and produces right inverse
#       as well as making sure get the cached data
test1 <- function() {
	m1 <- matrix(c(1,0,0,3,6,0,7,0,3), nrow= 3, ncol=3) # make a three by three matrix
	m1s1 <- solve(m1)									# get the inverse
	m1l <- makeCacheMatrix(m1)							# get the list 
	m1s2 <- cacheSolve(m1l)								# solve it through cacheSolve
	m1s3 <- cacheSolve(m1l)								# solve it again
														# NOTE: should see "getting cached data" printed out
	if (all(m1s2 == m1s1) & all(m1s3 == m1s1)) {
		print("test passed")
	}  else {
		print("test failed")
	}
}


# test2 checks that the caching actually saves time in large matrices
test2 <- function(matsize, iterations) { 
	# make a random matrix of n by n size
	testMatrix <- matrix(rnorm(matsize * matsize), nrow = matsize, ncol = matsize)
	testlist <- makeCacheMatrix(testMatrix)
	
	Sys.time()
	op <- options(digits.secs = 6)						# millisecs
	
	t1 <- Sys.time()
	temp <- cacheSolve1(testlist)
	t2 <- Sys.time()
	nocacheduration <- t2 - t1
	
	t1 <- Sys.time()
	for (i in 1: iterations) {
		temp <- cacheSolve1(testlist)
	}
	t2 <- Sys.time()
	cacheduration <- (t2 - t1) / iterations
	print(paste(c("Duration without caching: ", nocacheduration, "Duration with cachimg", cacheduration)))
}



# results:
#> test1()
#getting cached data
#[1] "test passed"
#
# test2(100,10)
#[1] "Duration without caching: " "0.0397319793701172"        
#[3] "Duration with cachimg"      "5.01871109008789e-05"      
#> test2(1000,10)
#[1] "Duration without caching: " "2.29858803749084"          
#[3] "Duration with cachimg"      "0"                         
#> test2(1000,10)
#[1] "Duration without caching: " "1.46213579177856"          
#[3] "Duration with cachimg"      "0"                         
#> test2(1000,100)
#[1] "Duration without caching: " "1.35254812240601"          
#[3] "Duration with cachimg"      "5.00917434692383e-06"  
#NOTE: the caching times which do not end up being zero are e-5 and e-6ne uses the original cacheSolve instead of the cacheSolve1 one would end up measuring the printing to stdout


