#Assignment 3
#Course: STAT 6861
#Instructor: Dr. Angell
#Due: 11/10/2014 

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
#   gen.perm
#
#   input, v, is a numeric vector
#   
#   output, p, is a matrix containing all possible permutations of the input vector
#
#   Source: This is example code provided by Dr. John Angell
#
#   Note: For this assignment I'm treating gen.perm as a blackbox. Comments describing the 
#         algorithm have been removed in the interest of brevity.
#
###############################################################


gen.perm <- function(v)
{ # gen.perm

  if ( !is.numeric(v) ) { stop('argument must be numeric\n') }
  if ( !is.vector(v) ) { stop('argument must be a vector\n') }
 
  n = length(v)
  if ( n < 1 ) { stop('argument length is 0\n') } 
  if ( n == 1) { return(as.matrix(v)) }

  if ( n > 10 ) { stop('we only allow lengths of 10 or less\n') }

  p = matrix(v[1], ncol = 1)

  for ( i in 2:n )
    { # i loop
       new.p = matrix( rep(0,prod(2:i)*i), ncol = i )
       
       num.c = ncol(p)
       num.r = nrow(p)
       offset = 0
       for ( j in 1:num.r)
         { # j loop
             t       = c(v[i],p[j,])
             for ( k in 1:i )
               { # for k
                 new.p[offset+k,] = t
                 temp             = t[k]
                 t[k]             = t[k+1]
                 t[k+1]           = temp
               } # for k 
             offset = offset + i   
         } # j loop
      p = new.p
    } # i loop
         
  return(p)  

} # gen.perm

###############################################################
#
#  Part I 
#  
#  ticket.line
#
#   inputs:
#    --n is the number of customers holding $5, and the number holding $10. There are therefore 2*n customers total
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

ticket.line <- function(n,sim.length)
{ #ticket.line

  if ( !is.valid.numeric(n) || !is.valid.numeric(sim.length) ) { stop("invalid input\n") }       #ensure arguments are valid numeric values
  if ( length(n) != 1 || length(sim.length) != 1 ) { stop("arguments must be of length = 1\n") } #ensure arguments are vectors of length one  
  if ( n < 1 || sim.length < 1 ) { stop("arguments must be positive integers\n") }               #ensure arguments are positive
  if ( n%%1 != 0 || sim.length%%1 != 0 ) { stop("arguments must be positive integers\n") }       #ensure arguments are integer values  


  customers = c(rep(1,n), rep(2,n))                                                              #represent $5's with 1 and $10's with 2
  all.served = numeric(sim.length)                                                               #initialize vector to track successful simulations

  for (i in 1:sim.length)                                                                        #each iteration simulates one line
  {#for
    line = sample(customers, 2*n)                                                                #create scrambled line (w/o replacement is the default) 
    
    change = 0                                                                                   #reset change count for each iteration 
    for (j in 1:(2*n))                                                                           #iterate through each customer
    { #for
      if (line[j] == 1) { change = change + 1 }                                                  #$5 bill as payment increment $5's count
      else { change = change - 1 }                                                               #$10 bill as payment decrement $5's count         
      if (change < 0) { break }                                                                  #break if change runs out
      if (j == length(line)) { all.served[i] = 1 }                                               #mark success if everyone is able to buy a ticket
    } #for
  } #for
  
  out = mean(all.served)                                                                         #calculate proportion of successes
  return(out)

} #ticket.line

###############################################################
#
#  Part II 
#  
#  ticket.line.perm
#
#   inputs:
#    --n is the number of customers holding $5, and the number holding $10. There are therefore 2*n customers total.
#        Note: n must range between 1 and 5 inclusive
#     
#   functionality:
#   Calculates the probability of serving an entire queue of customers of length 2*n. 
#   The hypothetical line is composed of an equal number of individuals holding $5 bills, and $10 bills
#   distributed randomly on the line. There is no outside or preexisting source of change so a customer
#   bearing a $10 bill will be turned away if the ticket seller runs out of $5 bills obtained from customers
#   paying with exact change. Considers all possible lines up to length 2*n (max 10), and reports the proportion
#   that are fully servicable. Unlike the function from part 1 this is not an estimate.
#
#   output:
#    --out is the proportion of potential ticket lines that can be fully served. Note that all potential permutations are considered.
#            
###############################################################

ticket.line.perm <- function(n)
{#ticket.line.perm

  if ( !is.valid.numeric(n) ) { stop("invalid input\n") }                             #ensure argument is a valid numeric value
  if ( length(n) != 1 ) { stop("argument must be of length = 1\n") }                  #ensure argument of length one  
  if ( n < 1 || n > 5 ) { stop("argument must be between one and five inclusive\n") } #ensure arguments are positive
  if ( n%%1 != 0 ) { stop("argument must be a positive integer\n") }                  #ensure arguments are integer values


  customers = c(rep(1,n), rep(2,n))                                                   #represent $5's with 1 and $10's with 2
  permutations = gen.perm(customers)                                                  #generate all permutations of the ticket line

  all.served = numeric(nrow(permutations))                                            #initialize vector to track successes

  for (i in 1:nrow(permutations))                                                     #iterate through each permutation
  {#for
    change = 0                                                                        #reset change count for each iteration
    for (j in 1:(2*n))                                                                #iterate through each customer
    { #for
      if (permutations[i,j] == 1) { change = change + 1 }                             #$5 bill as payment increment $5's count 
      else { change = change - 1 }                                                    #$10 bill as payment decrement $5's count
      if (change < 0) { break }                                                       #break if change runs out
      if (j == 2*n) { all.served[i] = 1 }                                             #mark success if everyone is able to buy a ticket
    } #for
  } #for
  
  out = mean(all.served)                                                              #calculate proportion of successes
  return(out)
}#ticket.line.perm


#################################################################
#
#example make take a couple minutes to run):
# for (i in 1:5){
# x = ticket.line(i,10000)
# y = ticket.line.perm(i)
# print(x)
# print(y)
# }
#
#################################################################


