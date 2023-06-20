
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



