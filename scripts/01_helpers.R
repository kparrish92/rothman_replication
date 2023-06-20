# Create a function to calculate Cohen's D 
effect_size = function(mean1, mean2, sd1, sd2)
{
  mean_diff = mean1 - mean2
  pooled_sd = sqrt(((sd1^2 + sd2^2)/2))
  es = mean_diff/pooled_sd  
  return(es)
}


mcmc_plot_function = function(mod)
{
  posterior <- as.data.frame(mod)
  
  pars = colnames(posterior[1:nrow(fixef(mod))])
  
  fixef_df = fixef(mod) %>% 
    as.data.frame() %>% 
    rownames_to_column("parameter") %>% 
    mutate(parameter = paste0("b_", parameter))
  
  plot = mcmc_areas(posterior, pars) +
    geom_text(data = mutate_if(fixef_df, is.numeric, round, 2),
              aes(label = paste0(Estimate, " [", Q2.5, "-", Q97.5, "]"), 
                  x = Inf,
                  family = "serif"), 
              hjust = "inward", size = 3) + 
    xlim(-5,5) +
    theme(text=element_text(size=10, family="serif")) +
    theme(axis.text.y= element_text(face = "plain"))
  return(plot)
}