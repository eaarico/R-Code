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
#  Part I 
#  
#  ticket.line
#
#   inputs:
#    --n is the number of customers holding $5, and the number holding $10. There are therefore 2*n customers total
#    
#    --change is the number of spare $5 bills held at the ticket booth at the start of the process (zero by default)
#     
#    --sim.length is the number of identical ticket lines to be simulated   
# 
#   functionality:
#   Estimates, using simulation, the probability of serving an entire queue of customers of length 2*n. 
#   The hypothetical line is composed of an equal number of individuals holding $5 bills, and $10 bills
#   distributed randomly on the line. There is no outside or preexisting source of change so a customer
#   bearing a $10 bill will be turned away if the ticket seller runs out of $5 bills obtained from customers
#   paying with exact change. 
#
#   output:
#    --out is the proportion of simulated tickets lines that can be fully served. 
#            
###############################################################

ticket.line <- function(n,change = 0,sim.length)
{ #ticket.line

  if ( !is.valid.numeric(n) || !is.valid.numeric(sim.length) || !is.valid.numeric(change) ) 
  { stop("invalid input\n") }                                                                    #ensure arguments are valid numeric values
  
  if ( length(n) != 1 || length(sim.length) != 1 || length(change) != 1) 
  { stop("arguments must be of length = 1\n") }                                                  #ensure arguments are vectors of length one  
  
  if ( n < 1 || sim.length < 1) 
  { stop("n and sim.length must be positive integers\n") }                                       #ensure arguments are positive


  if ( n%%1 != 0 || sim.length%%1 != 0 || change%%1 != 0 ) 
  { stop("arguments must be integers\n") }                                                       #ensure arguments are integer values  


  customers = c(rep(1,n), rep(2,n))                                                              #represent $5's with 1 and $10's with 2
  all.served = numeric(sim.length)                                                               #initialize vector to track successful simulations

  for (i in 1:sim.length)                                                                        #each iteration simulates one ticket line
  {#for
    line = sample(customers, 2*n)                                                                #create scrambled line (w/o replacement is the default) 
    
    change.count = change                                                                        #reset change count for each iteration 
    for (j in 1:(2*n))                                                                           #iterate through each customer
    { #for
      if (line[j] == 1) { change.count = change.count + 1 }                                      #$5 bill as payment increment $5's count
      else { change.count = change.count - 1 }                                                   #$10 bill as payment decrement $5's count         
      if (change.count < 0) { break }                                                            #break if change runs out
      if (j == length(line)) { all.served[i] = 1 }                                               #mark success if everyone is able to buy a ticket
    } #for
  } #for
  
  out = mean(all.served)                                                                         #calculate proportion of successes
  return(out)

} #ticket.line

############################################################### 
#  
#  k.nn(k,v.data,t.data)
#
#  functionality:
#  for each observation in the validation data set k.nn finds the k nearest neighbors in the test data set 
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
    
    dist.sqrd = numeric(mt)                                                               #initialize vector to hold distances
    nearest.k = numeric(k)                                                                #initialize vector to hold indices of 5 nearest neighbors 

    for (it in 1:mt)
    {#for
      dist.sqrd[it] = sum( (t.data[it,] - v.data[iv,])^2 )                                #calculate squared distances
    }#for
    
    for (ik in 1:k)                                                                          
    {#for
      nearest.k[ik] = which.min(dist.sqrd)                                                #Find index of nearest neighbor
      dist.sqrd[which.min(dist.sqrd)] = Inf                                               #Mask distance of nearest in order to find next nearest   
    }#for
    
    out[iv,] = nearest.k                                                                  #add indices of 5 nearest neighbors for current obs to output

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