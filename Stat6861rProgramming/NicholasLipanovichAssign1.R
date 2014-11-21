name = "Nicholas Lipanovich"
##Problem 1
####################################################################
#
#    function name: merge.sort
#        input arguments are in1,in2.
#                in1 and in2 must be numeric vectors
#                they must have a positive length
#               they must be sorted in ascending order
#
#        output is a vector containing the two vectors merged together and sorted.
#
####################################################################

merge.sort <- function(in1,in2)
{#checking if both vectors are numeric and are of length greater than zero
  if(length(in1) >= 1 && length(in2) >= 1 && is.numeric(in1) && is.numeric(in2))
  {#creates a new vector with a length that is the size of the lengths of the two vectors added together
    x = in1
    y = in2
    totalLen = (length(x) + length(y))
    z = NULL
    j = 1
    i = 1
    #checks which vector is the longer one and assigns the longer one to 'long' and the shorter to 'short'
    if(length(x) >= length(y))
    {
      m = length(y)
      n = length(x)
      long = x
      short = y
    }
    else
    {
      m = length(x)
      n = length(y)
      long = y
      short = x
    }
    #while each vector still has numbers left to sort goes into loop
    while(j <= m || i <= n)
    {#if the index of either of the vectors is na, it adds the number from the vector that still has numbers to the new vector z
      if(is.na(short[j]))
      {
        z = c(z, long[i])
        i = i + 1
      }
      else if(is.na(long[i]))
      {
        z = c(z, short[j])
        j = j + 1
      }
      #if both vectors are not na, it checks to see which number is shorter and stores it in the new vector z
      else if(long[i] >= short[j])
        
      {
        z = c(z, short[j])
        j = j + 1
      }
      else if(long[i] <= short[j])
      {
        z = c(z, long[i])
        i = i + 1
      }
    }
  #returns the new vector z
   return(z)
  }
  #if either of the vectors are not numeric or have a length of less than one, it stops function and prints an error statement
  else
  {
    stop("Error, at least one vector is either not numeric or is empty")
  }
}

##Problem 2
####################################################################
#
#    function name: bin.data
#        input arguments are x and bins.
#                x and bins must be numeric vectors
#                they must have a positive length
#               the bins vector must be sorted in ascending order
#                there cannot be any -Inf or Inf values in bins vector
#               
#
#        output is a vector containing a count of how many elements of x fall into each bin
#        This vector is of length one greater than that of the bins vector 
#
####################################################################
bin.data <- function(x, bins)
{#checking that both vectors have a length of one or greater and are numeric
  if(length(x) >= 1 && length(bins) >= 1 && is.numeric(x) && is.numeric(bins))
  {
    #checks if bins vector has any -Infs or Infs
    #if it does then the function will stop and an error message will be printed
    if(any(bins == Inf) || any(bins == -Inf))
    {
      stop("Error, bin values cannot be Inf or -Inf")
    }
    m = length(bins)
    n = length(x)
    countVal = numeric(m+1) #creating new output vector
    #for each number in vector x from 1 to n (the length)
    for(i in 1:n)
    {
      #for each number in vector bins from 1 to m (the length)
      #checks if number from vector x is smaller or equal to each bin value
      #if it is then it gets put in that bin then the loop breaks and i is incremented 
      #if it is larger than the last bin value then it gets put in the very last bin, the loop breaks, and i is incremented
      for(j in 1:m)
      { 
        if(x[i] <= bins[j])
        {
          countVal[j] = countVal[j] + 1
          break
        }
        else if(x[i] > bins[m])
        {
          countVal[m+1] = countVal[m+1] + 1
          break
        }
      }
    }
  return(countVal)
  }
  #if either of the vectors are not numeric or have a length of less than one, it stops function and prints an error statement
  else
  {
    stop("Error, at least one vector is either not numeric or is empty")
  }
}