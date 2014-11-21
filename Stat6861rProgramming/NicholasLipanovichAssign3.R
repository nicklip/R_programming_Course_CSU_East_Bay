name = "Nicholas Lipanovich"
##Assignment 3
##Problem 1
####################################################################
#
#    function name: ticket.line
#        Input arguments are n and sim.length.
#               n is a positive integer of length 1 and 2n is the amount of $5s and $10s to
#                have in the ticket line. sim.length is also a positive integer of length
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
ticket.line <- function(n, sim.length)
{##Error checking
  #checking that both sim.length and n are positive integers
  if(n <= 0 || sim.length <= 0 || n%%1 != 0 || sim.length%%1 != 0)
  {
    stop("Error: both of the input values must be positive integers")
  }
  #checking that both sim.length and n are of length 1
  if(length(n) != 1 && length(sim.length) != 1)
  {
    stop("Error: the lengths of each of the input values must be equal to 1 exactly")
  }
  #checking that both sim.length and n are finite(not NA, Nan, Inf, or -Inf) and numeric
  if(!is.numeric(n) || !is.numeric(sim.length) || !is.finite(n) || !is.finite(sim.length))
  {
    stop("Error: both of the input values must be finite, numeric values")
  }
  #initializing the variable that will the # of times the last customer was
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
    change = 0
    #while loop that acts as the ticket counter. As long as the ticket counter as change, it will
    #countinue to operate. If the order of the $5s and $10s is such that a customer needs 
    #change ($10) when the ticket counter doesn't have it, the loop will break and a new 
    #sample will be drawn. If the whole ticket line makes it through the ticket counter
    #successfully then the allCustPur variable (variable that holds how many lines make it 
    #through) will be incremented
    while(j <= length(samp))
    { 
      if(samp[j] == 5)
      {
        change = change + 1
        j = j + 1
      }
      else
      {
        change = change - 1
        j = j + 1
        if(change < 0)
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
#    function name: ticket.line.perm
#        Input argument is n.
#               n is a positive integer of length 1 and 2n is the amount of $5s and $10s to
#                have in the ticket line. 
#                n must be a numeric vector of value 1, 2, 3, 4, or 5, and cannot
#               have any NA, NaN, Inf, or -Inf values.
#                         
#       The output is a postive number that is the number of ticket lines (permuations) to
#       make it all the way through without the ticket counter needing more change than it has
#       divided by the number of rows in allPerms (# of permutations). This number can also 
#       be interpreted as the probability that every customer who lined up at the beginning 
#       of the process will be able to purchase a ticket.
#
####################################################################
ticket.line.perm <- function(n)
{##Error checking
  #checking that n is a positive integer that is of the value 1, 2, 3, 4, or 5.
  if(!is.numeric(n) || !is.finite(n) || n <= 0 || n%%1 != 0 || n > 5)
  {
    stop("Error: the input value must be a finite positive integer of the value 1, 2, 3, 4, or 5.")
  }
  #checking that n is of length 1
  if(length(n) != 1)
  {
    stop("Error: the length of the input value must be equal to 1 exactly")
  }
  #initializing the variable that will the # of times the last customer was
  #able to purchase a ticket and the variable that holds the dollar bill values
  #to permutate from. Also, sourcing the function gen.perm.R in order to use it.
  #source("C:/Documents and Settings/Nick/My Documents/Stat6861rProgramming/gen.perm.R")
  allCustPur = 0
  chooseFrom = length(2*n)
  #for loop to fill the variable to sample from with an equal amount of $5s and $10s
  for (i in 1:(2*n))
  {
    #if i is even then put a $10 bill in the variable to permutate from
    if(i%%2 == 0)
    {
      chooseFrom[i] = 10
    }
    #if i is odd then put a $5 bill in the variable to permutate from
    else
    {
      chooseFrom[i] = 5
    }
    
  }
  #Using the gen.perm function to generate all possible permutations of the values from chooseFrom
  allPerms = gen.perm(chooseFrom)
  #for loop to take each permutation from allPerms into the while loop and initialize the 
  #variables that are used each time in the while loop
  for(i in 1:NROW(allPerms))
  {
    samp = allPerms[i,]
    j = 1
    change = 0
    #while loop that acts as the ticket counter. As long as the ticket counter as change, it will
    #countinue to operate. If the order of the $5s and $10s is such that a customer needs 
    #change ($10) when the ticket counter doesn't have it, the loop will break and an another 
    #permutation will be drawn from allPerms. If the whole ticket line makes it through the ticket 
    #counter successfully then the allCustPur variable (variable that holds how many lines make it 
    #through) will be incremented
    while(j <= length(samp))
    { 
      if(samp[j] == 5)
      {
        change = change + 1
        j = j + 1
      }
      else
      {
        change = change - 1
        j = j + 1
        if(change < 0)
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
  #returns the allCustPur variable divided by the total number of permutations a.k.a. the  
  #probability that every customer who lined up at the beginning of the process will be able to 
  #purchase a ticket
  return(allCustPur/NROW(allPerms))
}
