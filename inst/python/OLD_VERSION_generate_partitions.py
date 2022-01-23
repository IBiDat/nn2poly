# Function that computes all the coefficients in a regression with p variables 
# up to order q_max and then generates all the partitions of those coefficientes.

# Import functions
from sympy.utilities.iterables import multiset_partitions
from itertools import combinations_with_replacement

#Define the function
def OLD_VERSION_generate_partitions(p,q_max):
  # List from 1 to p and 1 to q_max to iterate
  variables_list  = list(range(1,p+1))
  degrees_list = list(range(1,q_max+1))
  # Initialize the output:
  output = []
  # Iterate over the degrees
  for i in degrees_list:
    # Generate all the combinations for the coeffs of the given order
    comb = combinations_with_replacement(variables_list, i) 
    # Iterate over those combinations obtaining its partitions
    for j in list(comb):
      # Add each list of partitions to the output
      output.append(list(multiset_partitions(list(j))))
    
  return output    
    
    









