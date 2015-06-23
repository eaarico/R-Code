#Assignment 2
#Course: STAT 6861
#Instructor: Dr. Angell
#Due: 11/5/2014 

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
#   in.circle
#
#   inputs:
#    --pts is an n-by-2 matrix. 
#      Each row of the matrix represents a point in 2-dimentional space. There are therefore n points contained in the matrix.  
#      The first column contains the x coordinates of the points, and the second column contains the y coordinates.
#
#    --cntr is a vector of length 2. It is the center of a circle. 
#      cntr[1] is the x coordinate of the center of the circle, and cntr[2] is the y coordinate.
#
#    --r is a vector of length 1. It's value is the radius of the circle.  
#     
#   functionality:
#    in.circle determines which points in the input matrix 'pts' are contained in the specified circle.
#    Points exactly on the circle are considered inside the circle. Additionally, a plot of the circle and points is produced.
#    Points inside the circle are colored red, points outside the circle are colored blue.
#
#   output:
#    --inside is an m-by-2 matrix. It contains the subset of points from 'pts' that are inside the specified cirlce.
#      Each row of the matrix represents a point in 2-dimentional space. There are therefore m points contained in the matrix.
#      If there are no points within the circle in.circle returns NULL. 
#            
###############################################################

in.circle <- function(pts,cntr = c(0,0),r = 1)
{#in.circle
  
  if ( !is.valid.numeric(pts) || !is.valid.numeric(cntr) || !is.valid.numeric(r) )  
  { stop("invalid input\n") }                                    #ensure arguments contain valid values
  
  if ( !is.matrix(pts) ) { stop("pts not a matrix\n.") }         #ensure pts is a matrix
  
  inside = matrix(nrow=0,ncol=2)                                 #initialize matrix for points inside the circle
  outside = matrix(nrow=0,ncol=2)                                #initialize matrix for points outside the circle
  r.sqr = r^2                                                    #calculate & store square of the radius
  
  for (i in 1:nrow(pts))                                         #for loop inspects each point
  {#for
    if ((cntr[1] - pts[i,1])^2 + (cntr[2] - pts[i,2])^2 <= r.sqr)#test whether point lies within the circle
    {#if
      inside = rbind(inside, pts[i,]) 
    }#if
    
    else
    {#else
      outside = rbind(outside, pts[i,])
    }#else
  
  }#for

  
  #generate points for the circle
  theta = seq(0,2*pi,length = 2000)
  x.circle     = r*cos(theta) + cntr[1]
  y.circle     = r*sin(theta) + cntr[2]

   
  #determine limits for plotting
  if (min(x.circle) < min(pts[,1])) { x.min = min(x.circle) }
  else { x.min = min(pts[,1]) }

  if (max(x.circle) > max(pts[,1])) { x.max = max(x.circle) }
  else { x.max = max(pts[,1]) }

  if (min(y.circle) < min(pts[,2])) { y.min = min(y.circle) }
  else { y.min = min(pts[,2]) }

  if (max(y.circle) > max(pts[,2])) { y.max = max(y.circle) }
  else { y.max = max(pts[,2]) }

  dev.new()                                                        #open a new plotting window
  plot(x.circle, y.circle,                                         #plot circle
	type = 'l', 
	main = 'Plot',
	sub = 'Eric Arico',
	xlab = 'x', ylab ='y',
	xlim=c(x.min, x.max),                                      #specifiy appropriate x range
	ylim=c(y.min, y.max))                                      #specifiy appropriate y range
  abline( h = cntr[2])
  abline( v = cntr[1])
  points(inside[,1],inside[,2], col = 'red')                       #plot inside points
  points(outside[,1],outside[,2], col = 'blue')                    #plot outside points

  if (nrow(inside) == 0){ return(NULL) }                           #return NULL if no points lie inside the circle
  else { return(inside) }                                          #otherwise return matrix of points that lie inside the circle

}#in.circle


#
# Example
#
# set.seed(40)
# x = runif(14,0,10)
# x = round(x,3)
# y = runif(14,0,10)
# y = round(y,3)
# pts = cbind(x,y)
# print(pts)
# cntr = c(4,5)
# r = 3.5
# a = in.circle(pts,cntr,r)
# print(a)