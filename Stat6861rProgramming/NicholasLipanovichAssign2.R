name = "Nicholas Lipanovich"
##Assignment 2
####################################################################
#
#    function name: in.circle
#        input arguments are pts, cntr, and r.
#               pts is a nX2 matrix, cntr is the center of the circle and 
#               a vector of length 2, and r is the radius of the circle with 
#               a length of 1. 
#                pts, cntr, and r must be numeric vectors and cannot
#               have any NA, NaN, Inf, or -Inf values.
#                r must have a value of greater than 0.
#           
#         the output is a matrix containing the points from 'pts' that fall
#         within or on the line of the circle that was specified by 'cntr' and 'r'.
#         the output also includes a graph of the specified circle and all of the
#        points from 'pts'. The points that fall within the circle are colored red,
#         while the points that fall outside of the circle are colored blue.
#
####################################################################

in.circle <- function(pts,cntr,r)
{
  ##error checking
  #checking that r and cntr are of the correct length
  if(length(r) != 1 || length(cntr) != 2)
  {
    stop("Error: One of the inputs is of an incorrect length")
  }
  #checking that pts has exactly tow columns 
  if(!(NCOL(pts) == 2))
  {
    stop("Error: the input matrix must have exactly 2 columns")
  }
  #checking that pts, r, and cntr are all numeric
  if(!is.numeric(pts) || !is.numeric(r) || !is.numeric(cntr))
  {
    stop("Error: One of the inputs is not numeric")
  }
  #checking that the radius is of a value greater than zero
  if(r<=0)
  {
    stop("Error: The input radius cannot be zero or negative")
  }
  #checking that the center and radius don't have NAs, NaNs, Infs, or -Infs.
  if( !is.finite(r) || !is.finite(cntr[1]) || !is.finite(cntr[2]))
  {
    stop("Error: None of the inputs can contain NA, NaN, Inf, or -Inf values")
  }
  #constructing a circle using the specified radius and center
  theta = seq(0,2*pi,length = 2000)
  x = pts[,1]
  y = pts[,2]
  cirx = cntr[1] + r*cos(theta)
  ciry = cntr[2] + r*sin(theta)
  #initializing column variables that will be used in future matrices
  in.col1 = NULL
  in.col2 = NULL
  out.col1 = NULL
  out.col2 = NULL
  for(i in 1:length(x))
  {##Looping through all of the values in the matrix 'pts'
    #checking that the values in 'pts' don't have NAs, NaNs, Infs, or -Infs.
    if( !is.finite(x[i]) || !is.finite(y[i]))
    {
      stop("Error: None of the inputs can contain NA, Nan, Inf, or -Inf values")
    }
    #checking whether each point in 'pts' falls inside or outside of the 
    #specified circle
    if((x[i]-cntr[1])^2 + (y[i]-cntr[2])^2 <= r^2)
    {
      in.col1 = c(in.col1, x[i])
      in.col2 = c(in.col2, y[i])
    }
    else
    {
      out.col1 = c(out.col1, x[i])
      out.col2 = c(out.col2, y[i])
    }
  }
  #Making a matrix of the points that fell inside of the circle and a matrix
  #of the points that fell outside of the circle.
  inMatrix = cbind(in.col1, in.col2)
  outMatrix = cbind(out.col1, out.col2)
  #finding the x limits for the graph
  xmax = max(max(cirx), max(x))
  xmin = min(min(cirx), min(x))
  #finding the y limits for the graph
  ymax = max(max(ciry), max(y))
  ymin = min(min(ciry), min(y))
  #plotting the circle
  plot(cirx,ciry, type = 'l', xlab = 'x', ylab = 'y', 
       sub = "Nicholas Lipanovich", xlim = c(xmin, xmax), ylim = c(ymin, ymax))
  #plotting the points inside the circle with color red
  points(inMatrix[,1], inMatrix[,2], col="red")
  #plotting the points outside the circle with color blue
  points(outMatrix[,1], outMatrix[,2], col="blue")
  #returning the matrix of the points that fell inside the circle, if empty
  #then 'NULL' will be returned
  return(inMatrix)
}
