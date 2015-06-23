#Assignment 4
#Course: STAT 6861
#Instructor: Dr. Angell
#Due: 11/17/2014 

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
#  Part I 
#  
#  conv(x,y)
#
#  functionality:
#  Computes the convolution of two input vectors
#
#  inputs:
#   --x a numeric vector of length 1 or more 
#   --y a numeric vector of length 1 or more     
# 
#  output:
#   --z a numeric containing the convolution of x with y
#            
###############################################################

conv <- function(x,y)
{ #conv

  if ( !is.valid.numeric(x) || !is.valid.numeric(y) ) { stop("invalid input\n") }          #ensure arguments contain valid numeric values
  if ( length(x) < 1 || length(y) < 1 ) { stop("arguments must have positive length\n") }  #ensure arguments are vectors of length one  
  

  x.pad = c(rep(0, length(y) - 1),x,rep(0, length(y) - 1))                                 #create padded x vector
  y.rev = rev(y)                                                                           #create reversed y vector
  z = numeric(length(x) + length(y) - 1)                                                   #initialize output vector, z
  

  for (i in 1:length(z))                                                                   #populate output vector, z
  {#for
    z[i] = sum( y.rev[1:length(y)] * x.pad[i:(length(y) + i -1)] )                         #calculate appropriate i-th value of z
  } #for
  
  return(z)

} #conv



