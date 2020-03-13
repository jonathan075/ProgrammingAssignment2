##Based on the prior example of caching the mean of a vector, we are going to code two objects: the
#first one will store a special matrix and the second one will cach its inverse. The main goal is to
#optimize time.

##"makeCacheMatrix" is a function with a matrix as an argument, within its body
#we are going to define four functions. The first one will change the prior argument (x) to (y).
#The get function will return the stored matrix again. The last two functions are analogous to the explained, but
#will store its inverse. Finally the functions will be stored in a list.

makeCacheMatrix<-function(x=matrix()){
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function()inv
        
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


##Having our special matrix created, "cacheSolve" will use it to regain the inverse of "x".
#"inv" will call the getinverse function from above. If there is a result different of "NULL",
#we will get the cached matrix. Finally, with ih the last part the calculation is run and it will
#return "inv".


cacheSolve<-function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}


##Proving if the code is correct:
A<-makeCacheMatrix(matrix(c(4, 5, 3, 2), 2, 2))

A$get()

cacheSolve(A)



