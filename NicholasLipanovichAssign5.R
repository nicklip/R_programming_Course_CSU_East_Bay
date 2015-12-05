name = "Nicholas Lipanovich"
##Assignment 5
####################################################################
#
#    function name: detect.misclass
#         This function uses the k-nearest neighbors algorithm to check if any classified data points
#         have been misclassified
#        Input arguments are class.v and p
#               p is a matrix of the data points and class.v is a vector of classes that match
#               up with each data point in p
#               p must be a numeric, finite (not NA, Nan, Inf, or -Inf) matrix with at least 6 
#               rows and 2 columns
#               class.v must be a vector with length of at least 6
#               the length of class.v and the number of rows of p must be equal
#               
#                         
#         There are three different outputs, all of which are lists. err.found is a boolean vector that
#         is true if at least one data point has been misclassified or false if there are no 
#         misclassifications. new.class is a vector containing the indices of the rows of matrix p that 
#         are the misclassified data points. new.class is a vector containing the correct
#         classifications for the misclassified data points, as identified by the k-NN algorithm.
#
####################################################################
detect.misclass <- function(class.v, p)
{##Error checking
   #checking that p is finite(not NA, Nan, Inf, or -Inf), is numeric, and is a matrix that has
   #at least 6 rows and 2 columns
   if(!is.finite(p) || !is.numeric(p) || !is.matrix(p) || nrow(p) < 6 || ncol(p) < 2)
   {
     stop("Error: the input of data points needs to be a numeric, finite, matrix with at least 6
          rows and two columns")
   }
   #checking that class.v is a vector of length > 5
   if(!is.vector(class.v) || length(class.v) < 6)
   {
     stop("Error: the class vector must be a vector with a length of at least 6")
   }
   #checking that the length of the class.v vector and the number of rows in p are equal
   if(length(class.v) != nrow(p))
   {
     stop("Error: the class vector and the number of rows of the data points matrix must be of
          equal length")
   }
  #Initializing the output variables
  err.found = FALSE
  err.loc = NULL
  new.class = NULL
  #function to get the mode
  Mode <- function(x) 
  {
    #get unique values of x
    unq = unique(x)
    #using the indices of unq that match with x, finds the index that shows up the most and returns 
    #the value of that index from unq (the mode)
    return(unq[which.max(tabulate(match(x, unq)))])
  }
  
  for (i in 1:length(p[,1]))
  {
    #initializing the point that is going to be checked for misclassifications (can be multidimensional)
    pOfInter = NULL
    for(m in 1:ncol(p))
    { 
    pOfInter = c(pOfInter,p[i,m])
    }
    #a matrix that is the p matrix minus the point of interest
    pTwo = p[-i,]
    #initializing a vector of distances that are the distances from the point of interest to each 
    #of the other data points
    pTwoDist = NULL
    #using a for loop to compute each of the Euclidean distances and store them in vector pTwoDist
    for (k in 1:length(pTwo[,1]))
    { 
      newD = 0
      for (m in 1:ncol(p)){newD = newD + (pTwo[k,m] - pOfInter[m])^2} 
      newD2 = sqrt(newD)
      pTwoDist = c(pTwoDist,newD2)
    }
    #creating a matrix of the distances and each of the corresponding classes
    ptwoDistClass = cbind(pTwoDist, class.v[-i])
    #initializing the nearest neighbors matrix as the distances and classes of the first 5 data points
    Neighs = ptwoDistClass[1:5,]
    #taking out the nearest neighbors from the ptwoDistClass matrix
    ptwoDistClass = ptwoDistClass[-1:-5,]
    #comparing each of the data points from ptwoDistClass to the current nearest neighbors and trading
    #out the nearest neighbor with the greatest distance if it is greater than the data point distance
    for (j in 1:length(ptwoDistClass[,1]))
    {
      if(ptwoDistClass[j, 1] < max(Neighs[,1]))
      {
        Neighs = Neighs[-(which.max(Neighs[,1])),]
        Neighs = rbind(Neighs, ptwoDistClass[j,])
      }
    }
    #Finding the mode of the nearest neighbor classes 
    classOfKNN = Mode(Neighs[,2])
    #If the mode of the NN classes isn't the same as the class from the data point of interest, then
    #that point is considered misclassified and the outputs are reflected as such
    if (classOfKNN != class.v[i])
    {
      err.found = TRUE
      err.loc = c(err.loc, i)
      new.class = c(new.class, classOfKNN)
    }
  }
  #returning the output vectors
  return (list(err.found, err.loc, new.class))
}
