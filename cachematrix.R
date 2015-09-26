## This asignment creates two functions, the first one creates a special object that stores 
## both the matrix and its inverse (or NULL if not yet computed), the second one takes this
## object and gives back the inverse (if it is already there) or calculates and stores it

## makeCacheMatrix(x = matrix())
## This function takes a matris and returns a list of 4 functions, one to get the matrix back
## one to store a new matrix, one to store another matrix (the inverse) and one to take 
## the inverse back

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL             ##initialize inverse 
set<-function(y){     ##store the original matrix, if this is used then set inverse to NULL
  x<<-y
  inv<<-NULL
}
get<- function() x #                            #get the stored matrix
setinverse<-function(inverse) inv<<-  inverse   #store a matrix as inverse
getinverse<-function () inv                     #retrieve the inverse matrix
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) #return object list 
                                                                  # of functions to manipulate 
                                                                  #the matrix and its inverse
}

#cacheSolve(x, ...)
#This function tekes the object created with makeCacheMatrix and returns the inverse of 
#the matrix stored there, if the inverse is already there no calculation is done and the stored
#inverse is returned, else it is calculated and stored

cacheSolve <-function(x, ...){
  inv<-x$getinverse()               #try to get inverse from object
  if(!is.null(inv)){                #check if its there
    message("getting cached data")
    return(inv)                     #return inverse and message to user, stop function
  }
  data<-x$get() 
  inv<-solve(data,...)              #else get data and calculate inverse
  x$setinverse(inv)                 #store inverse in object
  inv                               #return inverse
}

