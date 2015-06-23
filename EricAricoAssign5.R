#Assignment 5
#Course: STAT 6861
#Instructor: Dr. Angell
#Due: 12/01/2014 

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
#  detect.misclass(class.v,p)
#
#  functionality:
#  Scans a dataset for misclassified observations 
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

detect.misclass <- function(class.v,p)
{ #detect.misclass

  if ( !is.valid.numeric(class.v) || !is.valid.numeric(p) ) { stop("invalid input\n") }     #ensure arguments contain valid numeric values
  if ( length(class.v) != nrow(p) ) { stop("length of class.v is not equal to nrow(p)\n") } #ensure arguments are vectors of length one

  m = length(class.v)                                                                       #determine number of observations
  n = ncol(p)                                                                               #determine number of measured attributes

  err.found = FALSE                                                                         #initialize error found flag
  err.loc   = numeric(0)                                                                    #initialize vector of indices of misclassed observations
  new.class = numeric(0)                                                                    #initialize vector of reclassifications  

  
  for (i in 1:m)                                                                            #for each observation
  {#for
    
    dist.sqrd = numeric(m)                                                                  #initialize vector to hold distances
    nearest.k = numeric(5)                                                                  #initialize vector to hold indices of 5 nearest neighbors 

    for (j in 1:m)
    {#for
      if (j != i) { dist.sqrd[j] = sum( (p[j,] - p[i,])^2 ) }                               #calculate squared distances
    }#for
    
    for (k in 1:5)                                                                          
    {#for
      nearest.k[k] = which.min(dist.sqrd)                                                   #Find index of nearest neighbor
      dist.sqrd[which.min(dist.sqrd)] = Inf                                                 #Mask distance of nearest in order to find next nearest   
    }#for

    class.nearest = class.v[nearest.k]                                                      #Find classes of nearest neighbors
    class.nearest.tbl = table(class.nearest)
    reclass = names(class.nearest.tbl[which.max(class.nearest.tbl)])                        #Set reclass to mode of class of neighbors

    if (reclass != class.v[i])                                                              #If reclass not equal orignal class
    {#if
     err.found = TRUE                                                                       #Set error flag to true
     err.loc = c(err.loc,i)                                                                 #Track error location
     new.class = c(new.class,reclass)                                                       #Specifiy correct class
    }#if  
  
  }#for

 
  if ( length(err.loc) == 0 )   {err.loc = NULL}                                           #If no misclassifications detected set err.loc to NULL
  if ( length(new.class) == 0 ) {new.class = NULL}                                         #If no misclassifications detected set new.class to NULL
  out.list = list(err.found,err.loc,new.class)                                             #Create list for output
  return(out.list)                                                                         

} #detect.misclass



