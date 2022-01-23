# Function that computes all the coefficients in a regression with p variables 
# up to order q_max and then generates all the partitions of those coefficientes.

# Import functions
from sympy.utilities.iterables import multiset_partitions
from itertools import combinations_with_replacement
from collections import Counter

#Define the function
def generate_partitions(p,q_max):
  # List from 1 to p and 1 to q_max to iterate
  variables_list  = list(range(1,p+1))
  degrees_list = list(range(1,q_max+1))
  # Initialize the output:
  output = []
  # Iterate over the degrees
  for i in degrees_list:
    # Generate all the combinations for the coeffs of the given order
    comb = combinations_with_replacement(variables_list, i) 
    # Iterate over those combinations obtaining its partitions when needed
    for j in list(comb):
      
      # Count the occurences of each variable in the combination
      occurrences = Counter(j)

      # Store occurrences as an array with the variables order.
      occurrences_array = []
      for k in variables_list:
        occurrences_array.append(occurrences[k])
      
      # Check if occurrences are sorted in descending order, which
      # eliminates the equivalent situations (i.e., 112 is equivallent to 223,
      # to 332, to 221 and so on)
      if all(occurrences_array[i] >= occurrences_array[i+1] for i in range(len(occurrences_array) - 1)):
        # Add the list of partitions for that combination to the output
        output.append(list(multiset_partitions(list(j))))
    
  return output    
