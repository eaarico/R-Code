#Assignment 6
#Course: STAT 6861
#Instructor: Dr. Angell
#Due: 12/08/2014 

name = "Eric Arico"

###############################################################
#
#   is.valid.numeric
#
#   input is a single object
#   the input can be a vector or matrix
#
#   returns false if:
#       object is not numeric mode
#       length of object is 0
#       nan.valid is FALSE and NaN values are detected
#       na.valid  is FALSE and NA  values are detected
#       inf.valid is FALSE and Inf or -Inf values are found.
#     
#   otherwise return true
#
#   Note: is.na returns TRUE if the object has an NaN value
#         is.nan returns false if the object has an NA value
#
#   Source: Adapted from Dr. Angell's example code
#
###############################################################

is.valid.numeric <- function(x,na.valid = FALSE, nan.valid = FALSE, Inf.valid = FALSE)
{ #begin is.valid.numeric
  if ( !is.numeric(x) )                            { cat("mode is not numeric\n"); return(FALSE) }
  if ( length(x) == 0 )                            { cat("length equals 0\n"); return(FALSE) }
  if ( nan.valid == FALSE && any(is.nan(x)) )      { cat("NaN value(s) detected\n"); return(FALSE) }
  if ( na.valid  == FALSE && any(is.na(x)) )       { cat("NA value(s) detected\n"); return(FALSE) }
  if ( Inf.valid == FALSE && any(is.infinite(x)) ) { cat("Inf value(s) detected\n"); return(FALSE) }
  return(TRUE)
} #end is.valid.numeric

############################################################### 
#  
#  k.nn(k,v.data,t.data)
#
#  functionality:
#  for each observation in the validation data set k.nn finds the k nearest neighbors in the training data set 
#
#  inputs:
#   -- class.v a numeric vector containing the class of each observation 
#   -- p a matrix containing the measured attributes for each observation     
# 
#  output:
#   -- out.list a list containing three items:
#      - err.found a logical value TRUE if one or more misclassifications are detected, FALSE otherwise
#      - err.loc a vector containing the indices of misclassified observations, NULL if none are detected
#      - new.class a vector containing the corrected classification for misclassed observations 
#            
###############################################################

k.nn <- function(k,v.data,t.data)
{#k.nn

  if ( !is.valid.numeric(v.data) || !is.valid.numeric(t.data) || !is.valid.numeric(k) ) 
  { stop("invalid input\n") }                                                             #ensure arguments contain valid numeric values
  
  if ( ncol(v.data) != ncol(t.data) ) 
  { stop("# of columns of v.data not equal to # of columns of t.data\n") }                #ensure data sets have equal number of measured attributes

  if ( k%%2 != 1 || k < 3 ) 
  { stop("k must be an odd integer 3 or greater\n") }                                     #ensure k is an odd integer 3 or greater
                                                                            
  n   = ncol(v.data)                                                                      #determine number of measured attributes
  mv  = nrow(v.data)                                                                      #determine number of observations in validation set
  mt  = nrow(t.data)                                                                      #determine number of observations in the training set
  out = matrix(nrow = mv, ncol = k)                                                       #initialize output matrix

  for (iv in 1:mv)                                                                        #for each observation
  {#for
    
    dist.sqrd = numeric(mt)                                                               #initialize vector to hold squared distances
    nearest.k = numeric(k)                                                                #initialize vector to hold indices of k nearest neighbors 

    for (it in 1:mt)
    {#for
      dist.sqrd[it] = sum( (t.data[it,] - v.data[iv,])^2 )                                #calculate squared distances
    }#for
    
    for (ik in 1:k)                                                                          
    {#for
      nearest.k[ik] = which.min(dist.sqrd)                                                #Find index of nearest neighbor
      dist.sqrd[which.min(dist.sqrd)] = Inf                                               #Mask distance of nearest in order to find next nearest   
    }#for
    
    out[iv,] = nearest.k                                                                  #add indices of k nearest neighbors for current obs to output

  }#for
                                           
  return(out)                                                                         

}#k.nn

############################################################### 
#  
#  vote(class.id,knn.out)
#
#  functionality:
#  identifies the class of the k nearest neighbors
#
#  inputs:
#   -- class.id a numeric vector containing the class id for each observation of the training set
#   -- knn.out should be the output from k.nn, that is, a matrix with # of rows equal to the number of observations in the validation set, and
#      k columns containing corresponding row numbers of the nearest neighbors from the training data set.
# 
#  output:
#   -- out a numeric vector of length equal to the number of rows in knn.out, each cell contains the "knn" classification for the corresponding 
#      observation from the validation data set 
#            
###############################################################

vote <- function(class.id,knn.out)
{ #vote
  
  n        = nrow(knn.out)
  k        = ncol(knn.out)
  out      = numeric(n)
  class.nn = numeric(k) 

  for ( i in 1:n )
  {#for
    class.nn     = class.id[knn.out[i,]]                                       #Find classes of nearest neighbors 
    class.nn.tbl = table(class.nn)
    out[i] = names(class.nn.tbl[which.max(class.nn.tbl)])                      #Set output of observation i to mode of class of k nearest neighbors
  }#for

  return(out)  

} #vote

###############################################################
#
# Begin script
#
###############################################################

setwd('C:/Users/Eric/Documents/CSU East Bay Coursework/Fall 2014/R Programming/') #set working directory

valid.data = read.table(file = 'validate.set.1.dat', as.is = TRUE)                 #read validation dataset from dat file
train.data = read.table(file = 'train.set.1.dat', as.is = TRUE)                    #read training dataset from dat file

valid.matrix = matrix(as.numeric(as.matrix(valid.data[-1,])), ncol = 3)            #convert to matrix removing 1st row so index corresponds to observation
train.matrix = matrix(as.numeric(as.matrix(train.data[-1,])), ncol = 3)            #convert to matrix removing 1st row so index corresponds to observation

##############################################################
#run algorithm with k=3
##############################################################

nn = k.nn(3,valid.matrix[,-1],train.matrix[,-1])                                   #find k nearest neighbors for each point in validation dataset
knn.class = vote(train.matrix[,1],nn)                                              #classify each point according to its k nearest neighbors

print("Misclassified points:\n")

for (i in 1:length(knn.class))
{#for
if ( knn.class[i] != valid.matrix[i,1] ) {print(valid.matrix[i,-1])}               #compare knn class with actual class, print coordinates of missclassfied points 
}#for

###############################################################
#run algorithm with k=9
###############################################################

nn = k.nn(9,valid.matrix[,-1],train.matrix[,-1])                                   #find k nearest neighbors for each point in validation dataset
knn.class = vote(train.matrix[,1],nn)                                              #classify each point according to its k nearest neighbors

print("Misclassified points:\n")

for (i in 1:length(knn.class))
{#for
if ( knn.class[i] != valid.matrix[i,1] ) {print(valid.matrix[i,-1])}               #compare knn class with actual class, print coordinates of missclassfied points 
}#for

###############################################################
#run algorithm with k=23
###############################################################

nn = k.nn(23,valid.matrix[,-1],train.matrix[,-1])                                  #find k nearest neighbors for each point in validation dataset
knn.class = vote(train.matrix[,1],nn)                                              #classify each point according to its k nearest neighbors

print("X,Y Coordinates of Misclassified Points:")

for (i in 1:length(knn.class))
{#for
if ( knn.class[i] != valid.matrix[i,1] ) {print(valid.matrix[i,-1])}               #compare knn class with actual class, print coordinates of missclassfied points 
}#for



