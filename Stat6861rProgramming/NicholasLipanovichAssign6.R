name = "Nicholas Lipanovich"

##Assignment 6
##Problem 1
#library(compiler)
#cmpfun(the function name)
####################################################################
#
#    function name: ticket.line
#        Input arguments are n, change, and sim.length.
#               n is a positive integer of length 1 and 2n is the amount of $5s and $10s to
#                have in the ticket line. change is a nonnegative integer of length 1 that is the number of 5  
#               dollar bills the ticket counter starts with. sim.length is also a positive integer of length
#               1 and is the number of simulations (samples to be taken) of ticket lines.
#                n and sim.length must be numeric vectors and cannot
#               have any NA, NaN, Inf, or -Inf values.
#                         
#         The output is a postive number that is the number of ticket lines to make it
#         all the way through without the ticket counter needing more change than it has
#         divided by sim.length (# of simulations). This number can also be interpreted as
#          the probability that every customer who lined up at the beginning of the process
#         will be able to purchase a ticket.
#
####################################################################
ticket.line <- function(n, change = 0, sim.length)
{##Error checking
  #checking that change, sim.length, and n are all finite(not NA, Nan, Inf, or -Inf) and numeric
  if(!is.numeric(n) || !is.numeric(change) || !is.numeric(sim.length) || !is.finite(n) || !is.finite(change) 
     || !is.finite(sim.length))
  {
    stop("Error: all of the input values must be finite, numeric values")
  }
  #checking that sim.length and n are both positive integers and that change is a nonnegative integer
  if(n <= 0 || change < 0 || sim.length <= 0 || n%%1 != 0 || change%%1 != 0 || sim.length%%1 != 0)
  {
    stop("Error: the input variables n and sim.length of must be positive integers and change must be a 
         nonnegative integer")
  }
  #checking that change, sim.length, and n are all vectors of length 1
  if(!is.vector(n) || !is.vector(change) || !is.vector(sim.length) || length(n) != 1 || length(change) != 1 || 
       length(sim.length) != 1)
  {
    stop("Error: each of the input values must be a vector of length 1")
  }
  #initializing the variable that will be the # of times the last customer was
  #able to purchase a ticket and the variable that holds the dollar bill values
  #to sample from
  allCustPur = 0
  chooseFrom = length(2*n)
  #for loop to fill the variable to sample from with an equal amount of $5s and $10s
  for (i in 1:(2*n))
  {
    #if i is even then put a $10 bill in the variable to sample from
    if(i%%2 == 0)
    {
      chooseFrom[i] = 10
    }
    #if i is odd then put a $5 bill in the variable to sample from
    else
    {
      chooseFrom[i] = 5
    }  
  }  
  #for loop to sample from the chooseFrom variable and initialize the variables that are used
  #each time in the while loop
  for(i in 1:sim.length)
  {
    samp = sample(chooseFrom, 2*n)
    j = 1
    change1 = change
    #while loop that acts as the ticket counter. As long as the ticket counter as change, it will
    #countinue to operate. If the order of the $5s and $10s is such that a customer needs 
    #change ($10) when the ticket counter doesn't have it, the loop will break and a new 
    #sample will be drawn. The amount of change that the ticket counter starts with is the amount of  
    #change specified by the 'change' input variable given by the user, which is the number of 5 dollar   
    #bills the ticket counter starts with. If the whole ticket line makes it through the ticket counter
    #successfully then the allCustPur variable (variable that holds how many lines make it 
    #through) will be incremented
    while(j <= length(samp))
    { 
      if(samp[j] == 5)
      {
        change1 = change1 + 1
        j = j + 1
      }
      else
      {
        change1 = change1 - 1
        j = j + 1
        if(change1 < 0)
        {
          break
        }
      }
      if(j > length(samp))
      {
        allCustPur = allCustPur + 1
      }
    }
  }
  #returns the allCustPur variable divided by the simulation length a.k.a. the probability 
  #that every customer who lined up at the beginning of the process will be able to 
  #purchase a ticket
  return(allCustPur/sim.length)
}
##Problem 2
####################################################################
#
#    function name: k.nn
#         This function uses the k-nearest neighbors regression algorithm by finding the classifications of the k 
#         nearest neighbors from the training data set for all of the "new" observations in the validation set. 
#        Input arguments are k, v.data and t.data
#               k is a positive, finite, integer that denotes the number 
#               of nearest neighbors to find. v.data is a dataframe that is the validation data set
#               of coordinates of the observations to be classified. t.data is the training data set, a 
#               dataframe of coordinates and corresponding classifications that are used to classify the validation
#               observations by using the k-NN of each validation observation. Both t.data and v.data can be multi-
#               dimensional but they must have an equal number of dimensions, ncol(t.data) equal to ncol(v.data).
#               
#         The output is a nrow(v.data) by k matrix. Each row of the output contains the observation numbers of the
#         k nearest neighbors from t.data for the corresponding observation in v.data. 
# 
# 
#
####################################################################
k.nn <- function(k, v.data, t.data)
{##Error checking
  #checking that v.data and t.data are data frames that have
  #at least 2 columns each. It also checks that v.data has least 1 row and t.data has at least k+1 rows
 if(!is.data.frame(v.data) || nrow(v.data) < 1 || ncol(v.data) < 2 ||
    !is.data.frame(t.data) || nrow(t.data) < (k+1) || ncol(t.data) < 2)
 {
   stop("Error: The training and validation data sets both need to be dataframes with at least 2
        columns, with least 1 row in the validation set and k+1 rows in the training set")
 }
 #checking that k is an odd, numeric, finite, integer vector of length 1 that is a value of at least 1
 if(!is.vector(k) || length(k) != 1 || !is.numeric(k) || !is.finite(k) || k%%2 == 0 || k%%1 != 0 || k < 1)
 {
   stop("Error: the number of nearest neighbors vector must be an odd, numeric, 
        finite, integer vector of length 1 that is a value of at least 1")
 }
 #checking that the number of columns in t.data and v.data are equal (they have the same # of dimensions)
 if(ncol(t.data) != ncol(v.data))
 {
   stop("Error: the number of columns in the training and validation data sets must be equal")
 }
  #Initializing the output variables
  class.v = t.data[,1]
  p = t.data[,2:3]
  knn.out = matrix(, nrow = nrow(v.data), ncol = k)
  for (i in 1:nrow(v.data))
  {
    #initializing the point that is going to be checked for misclassifications (can be multidimensional)
    pOfInter = NULL
    for(m in 2:ncol(v.data))
    { 
      pOfInter = c(pOfInter,v.data[i,m])
    }
    #a matrix that is the p matrix minus the point of interest
    #initializing a vector of distances that are the distances from the point of interest to each 
    #of the other data points
    pDist = NULL
    #using a for loop to compute each of the Euclidean distances and store them in vector pTwoDist
    for (h in 1:nrow(p))
    { 
      newD = 0
      for (m in 1:ncol(p)){newD = newD + (p[h,m] - pOfInter[m])^2} 
      pDist1 = sqrt(newD)
      pDist = c(pDist,pDist1)
    }
    #creating a matrix of the distances and each of the corresponding classes
    pDistClass = cbind(pDist, class.v)
    #initializing the nearest neighbors matrix as the distances and classes of the first k data points
    Neighs = cbind(pDistClass[1:k,], 1:k)
    #taking out the nearest neighbors from the pDistClass matrix
    pDistClass = pDistClass[-1:-k,]
    #comparing each of the data points from pDistClass to the current nearest neighbors and trading
    #out the nearest neighbor with the greatest distance if it is greater than the data point distance
    for (j in 1:nrow(pDistClass))
    {
      if(pDistClass[j, 1] < max(Neighs[,1]))
      {
        Neighs = Neighs[-(which.max(Neighs[,1])),]
        Neighs = rbind(Neighs, c(pDistClass[j,], (j+k)))
      }
    }
    knn.out[i,] = Neighs[,3]
  }
  #returning the output vectors
  return (knn.out)
}

####################################################################
#
#    function name: vote
#         This function assigns each of the new validation observations to the classification that the  
#         majority of the k nearest neighbors has. 
#        Input arguments are class.id and knn.out
#               knn.out is the output of the k.nn function, which is a matrix of the observation numbers of the
#               k nearest neighbors from t.data for the corresponding observations in v.data and class.id is 
#               a vector containing the classifications from the training data set. 
#                        
#               
#        The output is a vector of length nrow(knn.out) that is the new classifications assigned to each of the     
#         observations of the validation data set. 
# 
# 
#
####################################################################
vote <- function(class.id,knn.out)
{##Error checking
  #checking that knn.out is a finite(not NA, Nan, Inf, or -Inf), numeric, matrix that has at least
  #one column and two rows
  if(!is.finite(knn.out) || !is.numeric(knn.out) || !is.matrix(knn.out) || nrow(knn.out) < 2 ||ncol(knn.out) < 1)       
  {
    stop("Error: The output of the k.nn function needs to be a numeric, finite, matrix that has at least one 
         column and two rows")
  }
  #checking that class.id is a vector that has a length of at least 2 
  if(!is.vector(class.id) || length(class.id) < 2)
  {
    stop("Error: the classifications vector must be a vector that has length of at least 2")
  }
  #initializing the output vector
  outputVec = NULL
  #function to get the mode
  Mode <- function(x) 
  {
    #get unique values of x
    unq = unique(x)
    #using the indices of unq that match with x, finds the index that shows up the most and returns 
    #the value of that index from unq (the mode)
    return(unq[which.max(tabulate(match(x, unq)))])
  }
  #gets what the majority of the k-NN classifications are for each validation observation
  for(i in 1:nrow(knn.out))
  {
    #initializing the classifications vector of the row
    getclasses = NULL
    #gets each of the classifications from the k nearest neighbors and stores them in getclasses
    for(j in 1:ncol(knn.out))
    {
      getclasses = c(getclasses, class.id[knn.out[i,j]])
    }
    #gets the mode of the k nearest neighbors' classifications and stores it in the output vector
    outputVec = c(outputVec, Mode(getclasses))
  }
  #returns the vector that is the classifications for the validation observations
  return(outputVec)
}



