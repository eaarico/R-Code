############################################
#
#     Test environment for Assignment 5
#
#            Detect Misclassification
#
############################################

#   Need to install packages 'mvtnorm' to generate MVN data
#   Need to install 'rgl' to plot 3-D data

library(mvtnorm)

####################################################################
#####    start with 2-D ####################

###   You can set the S matrix to anything.
###   You could use runif(4,1,
S = matrix( c(4,2,1,5), ncol = 2 )  # vector can be anything



###   Ensure S positive definite

S = t(S) %*% S + 0.1*diag(rep(1,2))


# set n1,n2,mu1,mu2,min.errors, and max.errors as desired


n1 = 23             # number of points in class 1
n2 = 15             # number of points in class 2
mu1 = c(9,2)        # POPULATION (not sample) mean for class 1
mu2 = c(23,45)      # ditto for class 2

max.errors = 3      # set the maximum number of misclassifications
min.errors = 0      # set the minimum number of misclassifications

if ( min.errors > max.errors )
  {
     cat("What are you thinking?\n")
  }

if ( max.errors > 0.1*(n1+n2) )
  {
     cat("Warning. Error rate exceeds 10 per cent.\n")
  }
####   Use common covariance matrix. (Not necessary, but simpler.)

class.1 = rmvnorm(n1,mu1,S)
class.2 = rmvnorm(n2,mu2,S)

class.vec = c( rep(1,n1), rep(2,n2) )  # assign classification
p         = rbind(class.1,class.2)     # combine both classes into a matrix

####    now permute the rows of class.vec and p using
####    the sample() function to permute the sequence 1:(n1+n2)
r       = sample( 1:(n1+n2),n1+n2, replace = FALSE) 

#####  these 3 lines are for deug in case permutation
#####  dores not work
#plot(rbind(class.1,class.2), type = 'n')  #set plot limits
#points(class.1, col = 'blue')
#points(class.2, col = 'red')

#####    Now permute the data
class.vec = class.vec[r]
p         = p[r,]

class.vec.error.free = class.vec  # save pristine classification
                                  # for comparison with code output

if ( .Platform$GUI == "Rgui" ) { dev.new() }

###   plot the points to see if the data with no
###   errors in the classification are well-separated
###      Note: We could test for separation algorithmically
###      using k nearest neighbors
plot(p, type = 'n')    # this automatically sets the limits        
points(p[which(class.vec == 1),], col = 'blue')
points(p[which(class.vec == 2),], col = 'red')

# Now choose a point or points to misclassify (and perhaps no point)

#   Set number of points to be misclassified
num.wrong = sample( min.errors:max.errors, 1) # choose a number from min to max
print(num.wrong)
#   now choose which points
if ( num.wrong > 0 )
  {
    wrong.loc = sample( 1:(n1+n2),num.wrong, replace = FALSE )
    wrong.loc = sort(wrong.loc)
    print(wrong.loc)
    class.vec[wrong.loc] = (class.vec[wrong.loc] %% 2) + 1  # tricky,tricky
    ######   make sure there is no error in the test environment  #####
    ######   should print only TRUE
    which(class.vec != class.vec.error.free) == wrong.loc
  }

#########  Strictly speaking, it is not necessary to plot, but
#########  it is nice to visualize the situation

if ( .Platform$GUI == "Rgui" ) { dev.new() }
plot(p, type = 'n', main = "Errors Introduced")    # this automatically sets the limits        
points(p[which(class.vec == 1),], col = 'blue')
points(p[which(class.vec == 2),], col = 'red')

###  at this point one can run the detect.miclass function

# result = detect.misclass(class.vec,p)

#  check that the function worked


####################################################################
#
#        3 D    Tests
#
####################################################################

library(rgl)   # for 3-D plots


###   You can set the S matrix to anything.
###   You could use runif(4,1,
S = matrix( c(runif(9,0,5)), ncol = 3 )  # vector can be anything



###   Ensure S positive definite

S = t(S) %*% S + 0.3*diag(rep(1,3))

n1 = 38             # number of points in class 1
n2 = 25             # number of points in class 2
mu1 = c(9,2,5)        # POPULATION (not sample) mean for class 1
mu2 = c(28,45,63)      # ditto for class 2

max.errors = 3      # set the maximum number of misclassifications
min.errors = 0      # set the minimum number of misclassifications

if ( min.errors > max.errors )
  {
     cat("What are you thinking?\n")
  }

if ( max.errors > 0.1*(n1+n2) )
  {
     cat("Warning. Error rate exceeds 10 per cent.\n")
  }
####   Use common covariance matrix. (Not necessary, but simpler.)

class.1 = rmvnorm(n1,mu1,S)
class.2 = rmvnorm(n2,mu2,S)

class.vec = c( rep(1,n1), rep(2,n2) )  # assign classification
p         = rbind(class.1,class.2)     # combine both classes into a matrix

####    now permute the rows of class.vec and p using
####    the sample() function to permute the sequence 1:(n1+n2)
r       = sample( 1:(n1+n2),n1+n2, replace = FALSE) 

#####  these 3 lines are for deug in case permutation
#####  dores not work
#plot3d(rbind(class.1,class.2), type = 'n')  #set plot limits
#points3d(class.1, col = 'blue')
#points3d(class.2, col = 'red')

#####    Now permute the data
class.vec = class.vec[r]
p         = p[r,]

class.vec.error.free = class.vec  # save pristine classification
                                  # for comparison with code output



###   plot the points to see if the data with no
###   errors in the classification are well-separated
###      Note: We could test for separation algorithmically
###      using k nearest neighbors
open3d()   # like dev.new()
plot3d(p, type = 'n', main = "No Error")    # this automatically sets the limits        
points3d(p[which(class.vec.error.free == 1),], col = 'blue')
points3d(p[which(class.vec.error.free == 2),], col = 'red')

# Now choose a point or points to misclassify (and perhaps no point)

#   Set number of points to be misclassified
num.wrong = sample( min.errors:max.errors, 1) # choose a number from min to max
print(num.wrong)
#   now choose which points
if ( num.wrong > 0 )
  {
    wrong.loc = sample( 1:(n1+n2),num.wrong, replace = FALSE )
    wrong.loc = sort(wrong.loc)
    print(wrong.loc)
    class.vec[wrong.loc] = (class.vec[wrong.loc] %% 2) + 1  # tricky,tricky
    ######   make sure there is no error in the test environment  #####
    ######   should print only TRUE
    which(class.vec != class.vec.error.free) == wrong.loc
  }

#########  Striuctly speaking, it is not necessary to plot, but
#########  it is nice to visualize the situation

open3d()
plot3d(p, type = 'n', main = "Errors Introduced")    # this automatically sets the limits        
points3d(p[which(class.vec == 1),], col = 'blue')
points3d(p[which(class.vec == 2),], col = 'red')

###  at this point one can run the detect.miclass function

# result = detect.misclass(class.vec,p)

#  check that the function worked




############################################################
#
#      higher dimension data tests can be done here.
#
#      everything except the plots will be the same.
#
#      One can plot any two or three dimensions. But the plot
#      could be misleading.
#
#############################################################





