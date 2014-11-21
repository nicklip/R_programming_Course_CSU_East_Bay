name = "Nicholas Lipanovich"
##Assignment 4
####################################################################
#
#    function name: conv
#        Input arguments are x and y.
#               x and y are numeric, finite (not NA, Nan, Inf, or -Inf), vectors 
#               that both have lengths that are greater than zero.
#               They are the two vectors that we want to convolute into one vector.
#                         
#         The output is a numeric vector, z, that is the resultant vector
#         from the convolution of x and y.
#
####################################################################
conv <- function(x, y)
{##Error checking
  #checking that x and y are both finite(not NA, Nan, Inf, or -Inf) and 
  #numeric vectors
  if(!is.vector(x) || !is.vector(y) || !is.finite(x) || !is.finite(y)
     !is.numeric(x) || !is.numeric(y))
  {
    stop("Error: Both inputs need to be numeric, finite, vectors")
  }
  #checking that x and y both have lengths that are greater than zero
  if(length(x) <= 0 || length(y) <= 0)
  {
    stop("Error: Both inputs must have a length of greater than zero")
  }
  #Finding which vector is shorter and which is longer
  if (length(x) > length(y))
  {
    longvar = x
    shortvar = y
  }
  else
  {
    longvar = y
    shortvar = x
  }
  #getting the length of the short and long vectors, initializing the output vector,
  #and 'flipping' the shorter vector
  n = length(longvar)
  m = length(shortvar)
  z = numeric(n + m - 1)
  shortvar = rev(shortvar)
  #padding the longer vector with m-1 zeros on both sides
  for(i in 1:(m-1))
  {
    longvar = append(longvar, 0)
    longvar = append(longvar, 0, after=0)
  }
  #using two for loops to fill the vector z with the convolution of x and y. To do
  #this I move the longer vector to the left after each for(j in 1:m) loop
  for(i in 1:length(z))
  {
    for(j in 1:m)
    {
      z[i] = z[i] + shortvar[j]*longvar[j+(i-1)]
    }
  }
  #return the resultant vector, z, from the convolution of x and y
  return(z)
}
