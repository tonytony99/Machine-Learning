
# Tolerance for floating point comparison
epsilon = 1e-6

# Given a matrix, inequalities, of the form
# a1,1 a1,2 ... a1,n b1
# ...
# am,1 am,2 ... am,n bm
# And a list, solutions, of the form
# x1, x2, ... , xn

# Return TRUE if all of the inequalities below hold, FALSE otherwise
# a1,1 * solution[1] + a1,2 * solution[2] + ... a1,n * solution[n] <= b1
# ...
# am,1 * solution[1] + am,2 * solution[2] + ... am,n * solution[n] <= bm

# EXAMPLE
# inequalities = matrix(data = c(3,2,4,5,1,6), nrow = 2, byrow = TRUE)
# solution = c(2,1)
# holds(inequalities,solution) # FALSE
# solution = c(-8,1)
# holds(inequalities,solution) # TRUE

holds = function(inequalities,solution) {
  m = nrow(inequalities)
  n = ncol(inequalities) - 1
  for (i in 1:m){
    # If we find an unsatisfied inequality then return FALSE
    if (sum(inequalities[i,1:n]*solution) > inequalities[i,n+1] + epsilon) {
      return(FALSE)
    }
  }
  # No inequality is unsatisfied so return true
  return(TRUE)
}

# Given a matrix of the form
# a1 b1
# ...
# am bm
# Return a value x such that 
# a1 x <= b1
# ...
# am x <= bm

# EXAMPLE
# inequalities = matrix(data = c(2,3,-4,-5,1,6), ncol = 2, byrow = TRUE)
# satisfy(inequalities) # 1.5

satisfy = function(inequalities) {
  
  lMin = Inf
  gMax = - Inf
  negCoeff = posCoeff = 0
  
  m = nrow(inequalities)
  
  for (i in 1:m){
    
    if(inequalities[i,1] < 0){
      negCoeff = negCoeff + 1
      gMax = max(gMax,inequalities[i,2]/inequalities[i,1])
    }
    
    if(inequalities[i,1] > 0){
      posCoeff = posCoeff + 1
      lMin = min(lMin,inequalities[i,2]/inequalities[i,1])
    }
    
    if(inequalities[i,1] == 0 && inequalities[i,2] < 0) {
      return(NULL)
    }
  }
  
  # If all coefficients are positive then return the lowest RHS
  if (posCoeff > 0 && negCoeff == 0){
    return(lMin)
  }
  
  # If all coefficients are negative then return the highest RHS
  if (posCoeff == 0 && negCoeff > 0){
    return(gMax)
  }
  
  # If there are a mix of coefficients then return a value x such that
  # highest RHS for negative coeff <= x <= lowest RHS for positive coefficient
  if (posCoeff > 0 && negCoeff > 0){
    if(gMax <= lMin + epsilon){
      return(lMin)
    }
    else {
      # Highest RHS for negative coeff > lowest RHS for positive coefficient
      # so there are no possible solutions
      return(NULL)
    }
  }
}

# Given a matrix, inequalities, of the form
# a1,1 a1,2 ... a1,n b1
# ...
# am,1 am,2 ... am,n bm

# Return a list, of the form
# x1, x2, ... , xn
# Such that
# a1,1 * solution[1] + a1,2 * solution[2] + ... a1,n * solution[n] <= b1
# ...
# am,1 * solution[1] + am,2 * solution[2] + ... am,n * solution[n] <= bm

# EXAMPLE
# E = c(c(1,2,-4,6),c(-5,3,2,3),c(3,-2,4,8),c(5,-9,1,8),c(-4,-9,1,8))
# E = matrix(data = E, nrow = 4, ncol = 4, byrow=TRUE)
# solution = fourierMotzkin(E)
# hold(E,solution) # TRUE

fourierMotzkin = function(inequalities) {
  Slist = list() # A list to hold systems of inequalities x <= ...
  Tlist = list() # A list to hold systems of inequalities x >= ...
  Elist = list() # A list to hold systems of inequalities a1 ... an <= b 
  
  Elist[[1]] = inequalities
  
  print(inequalities)
  
  V = ncol(inequalities) - 1
  n = ncol(inequalities) - 1
  
  # For variables x1 ... xn-1
  for (v in 1:(n-1)) {
    # counters
    ge = le = numInequalities = 0
    T = S = newInequalities = NULL
    
    # For each inequality
    for (i in 1:nrow(inequalities)){
      # If the coefficient of xv is 0 
      # Then add the inequality directly to the next system of inequalities
      if(inequalities[i,1] == 0) {
        inequality = inequalities[i,2:ncol(inequalities)]
        newInequalities = c(newInequalities, inequality)
        numInequalities = numInequalities + 1
      }
      # If the coefficient of xv > 0 
      # Then add the inequality to S (rearranged in terms of x)
      if(EMatrix[i,1] > 0) {
        # only variable coefficients are subtracted
        inequality = -inequalities[i,2:(ncol(inequalities)-1)]/inequalities[i]
        inequality = c(inequality, inequalities[i,ncol(inequalities)]/inequalities[i])
        S = c(S, inequality)
        le = le + 1
      }
      # If the coefficient of xv < 0 
      # Then add the inequality to T (rearranged in terms of x)
      if(inequalities[i,1] < 0) {
        # Only variable coefficients are subtracted
        inequality = -inequalities[i,2:(ncol(inequalities)-1)]/inequalities[i]
        inequality = c(inequality, inequalities[i,ncol(inequalities)]/inequalities[i])
        T = c(T, inequality)
        ge = ge + 1
      }
    }
    
    TMatrix = matrix(data = T, nrow = ge, ncol = V-v+1, byrow=TRUE)
    SMatrix = matrix(data = S, nrow = le, ncol = V-v+1, byrow=TRUE)
    
    Tlist[[v]] = TMatrix
    Slist[[v]] = SMatrix
    
    # For every combination of inequality from S and T
    for (t in 1:nrow(TMatrix)){
      for (s in 1:nrow(SMatrix)){
        # Because T <= x <= S, it's the case that x <= S - T
        inequality = TMatrix[t,1:(ncol(TMatrix)-1)]-SMatrix[s,1:(ncol(TMatrix)-1)]
        inequality = c(inequality,SMatrix[s,ncol(SMatrix)]-TMatrix[t,ncol(TMatrix)]) 
        newInequalities = c(newInequalities, inequality)
        numInequalities = numInequalities + 1
      }
    }
    
    newInequalities = matrix(data = newInequalities, nrow = numInequalities, ncol = V-v+1, byrow=TRUE)
    inequalities = newInequalities
    Elist[[v+1]] = inequalities
    print(inequalities)
  }
  
  # By this point we have eliminated every variable except from xn
  
  # Create a vector of length n to hold the value for x1 ... xn
  solution = rep(0,V)
  
  # Find a value for xn
  x = satisfy(Elist[[V]])
  solution[V] = x
  
  if (is.null(x)) {return(NULL)}
  
  # Now go back through previous systems of inequalities
  # each time using all the variables that we have values for so far
  # to reduce the system of inequalities to one solvable by the satisfy function
  
  numVariables = 2
  
  for (i in (V-1):1){
    newLHS = Elist[[i]][,1]
    newRHS = Elist[[i]][,V-i+2]
    currentValue = 0
    for (j in numVariables:1){
      newRHS = newRHS - solution[V-currentValue] * Elist[[i]][,j]
      currentValue = currentValue + 1
    }
    
    Elist[[i]] = matrix(c(newLHS, newRHS),ncol = 2)
    x = satisfy(Elist[[i]])
    solution[i] = x
    if (is.null(x)) {return(NULL)}
    numVariables = numVariables + 1
  }
  print("Solution : ")
  print(solution)
  return(solution)
}