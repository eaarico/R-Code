#Assignment 1
#Course: STAT 6861
#Instructor: Dr. Angell
#Due: 10/27/2014 

name = "Eric Arico"

###############################################################
#
#   Part 0
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
#   Part 1
#
#   merge.sort
#
#   input is a pair of *presorted* numeric vectors
#    
#   input vectors are combined into a single sorted numeric vector
#   taking advantage of initial order
#    
#   output is a single sorted numeric vector
#     
###############################################################

merge.sort <- function(in1,in2)
  {                                                                      #begin merge.sort

  if ( !is.valid.numeric(in1) || !is.valid.numeric(in2) )  { stop("invalid input\n.") }      #ensure arguments contain valid values
  if ( !is.vector(in1) || !is.vector(in2) )                { stop("input not a vector\n.") } #ensure arguments are, in fact, vectors      
  if (is.unsorted(in1) || is.unsorted(in2))                { stop("unsorted input\n.") }     #ensure arguments are, in fact, sorted
                                
  out = numeric(length(in1) + length(in2))                            #initialize output vector
  in1.index = 1; in2.index = 1; out.index = 1;                        #initialize indices   
  while (in1.index <= length(in1) && in2.index <= length(in2))        #while vectors 1 & 2 not exausted compare them and take lowest 
  {                                                                   #begin while
    if (in1[in1.index] <= in2[in2.index])
    {                                                                 #begin if
      out[out.index] = in1[in1.index]
      in1.index = in1.index + 1
    }                                                                 #end if

    else
    {                                                                 #begin else
      out[out.index] = in2[in2.index]
      in2.index = in2.index + 1
    }                                                                 #end else
      out.index = out.index + 1     
    }                                                                 #end while

    while (in1.index <= length(in1))                                  #while vector 1 not exhausted copy to output 
    {                                                                 #begin while
      out[out.index] = in1[in1.index]
      in1.index = in1.index + 1
      out.index = out.index + 1
    }                                                                 #end while

    while (in2.index <= length(in2))                                  #while vector 1 not exhausted copy to output 
    {                                                                 #begin while
      out[out.index] = in2[in2.index]
      in2.index = in2.index + 1
      out.index = out.index + 1
    }                                                                 #end while

    return(out) 

}                                                                     #end merge.sort

#example
#x = c(1,2,3,4); y = c(1.5,3,5)
#z = merge.sort(x,y)
#print(z)

##############################################################
#
#   Part 2
#
#   bin.data
#
#   inputs are: x, a vector of values to be "binned", and bins, a *strictly increasing* vector demarcating the bins
#   
#   output is a vector of length bins+1 
#   the values of the output vector represent the count of x values which fall into each bin  
#
##############################################################

bin.data <- function(x, bins)
{                                                                        #begin bin.data

if ( !is.valid.numeric(x) || !is.valid.numeric(bins) )  { stop("invalid input\n.") }                      #ensure arguments contain valid values
if ( !is.vector(x) || !is.vector(bins) )                { stop("input not a vector\n.") }                 #ensure arguments are vectors      
if ( is.unsorted(bins, strictly = TRUE) )               { stop("bins input not strictly increasing\n.") } #ensure bins argument is sorted
 
out = numeric(length(bins) + 1)                                          #initialize output vector

if (length(bins) > 1)
{                                                                        #begin if
  out[1] = sum(x <= bins[1])                                             #generate count for 1st bin

  for (i in 2:length(bins))                                              #generate counts for intervening bins
  {                                                                      #begin for loop
    out[i] = sum(x > bins[i - 1] & x <= bins[i])                      
  }                                                                      #end for loop
  
    out[length(bins)+1] = sum(x > bins[length(bins)])                    #generate count for last bin
    return(out)
  }                                                                      #end if
    
  if (length(bins) == 1)
  {                                                                      #begin if
    out[1] = sum(x <= bins[1])                                           #generate count for 1st bin                                      
    out[2] = sum(x > bins[1])                                            #generate count for 2st bin
    return(out) 
  }                                                                      #end if
    
}                                                                        #end bin.data

#example
#x = c(8.3,-2,2.3,7.9,2.5,2.51,8.5,-8.9,9.2)
#bins = c(2.5,5,7.8,9)
#print(bin.data(x, bins))