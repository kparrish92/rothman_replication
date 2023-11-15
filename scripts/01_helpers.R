# ------------------------------------------------------
# Author: Kyle Parrish
# Date 11/9/23
# This script stores and loads helper functions used in data
# tidying and analysis
# -------------------------------------------------------

# Create a function to calculate Cohen's D assuming an equal sample size
effect_size = function(mean1, mean2, sd1, sd2)
{
  mean_diff = mean1 - mean2
  pooled_sd = sqrt(((sd1^2 + sd2^2)/2))
  es = mean_diff/pooled_sd  
  return(es)
}

