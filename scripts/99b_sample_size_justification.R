# ------------------------------------------------------
# Date 11/9/23
# This script simulates data for the purpose of determining 
# the needed sample size  
# -------------------------------------------------------


# Code for a two sample power analysis that runs both a t.test and tost

#number of iterations 
k = 1000

temp_df = matrix(nrow = k, ncol = 2)
outer_df = matrix(nrow = 7, ncol = 3)
sizes = c(60,70,80,90,100,110,120)

for(thisSize in 1:nrow(outer_df)){
  
  
  for(thisRun in 1:k){
    mv1 = rnorm(95, m = 3.2, sd = 1.3)
    mv2 = rnorm(115, m = 3.2, sd = 1.3)
    tost = TOSTER::TOSTtwo(m1 = mean(mv1), m2 = mean(mv2), sd1 = sd(mv1), sd2 = sd(mv2), 
                   n1 = sizes[thisSize], n2 = sizes[thisSize], low_eqbound_d = -.4, high_eqbound_d = .4, alpha = .05,
                   plot = TRUE, verbose = TRUE)
    t_test_df = t.test(mv1, mv2, paired = FALSE)
    temp_df[thisRun, 1] = pmax(tost$TOST_p1, tost$TOST_p2)
    temp_df[thisRun, 2] = t_test_df$p.value
    temp_df = as.data.frame(temp_df) 
    
  }
  
  outer_df[thisSize, 1] = sizes[thisSize]
  outer_df[thisSize, 2] = sum(temp_df$V1 < .05) # tost positives
  outer_df[thisSize, 3] = sum(temp_df$V2 < .05) # (false) t-test positives 
  
}

# tidy results of loop to make the output readable 

sample_needed = as.data.frame(outer_df) %>% 
  rename("Sample Size" = V1) %>% 
  mutate(V2 = V2/1000) %>% 
  mutate(V3 = V3/1000) %>% 
  rename("Pct Positive TOST" = V2) %>% 
  rename("Pct Positive T-test" = V3)
  

# Sensitivity analysis for review 

pwr::pwr.2p2n.test(h = NULL, n1 = 96, n2 = 115, sig.level = .05, power = .8)

TOSTER::powerTOSTtwo(alpha=0.05, N = 105, statistical_power=0.8) # d = .4038 

sd_present = round(mean(c(d_df_i$sd_corr, d_df$sd_corr)), digits = 1) # pull sd from the study

sd_present*.0038*20 # Multiply by 20 to get from correct answers to percent correct