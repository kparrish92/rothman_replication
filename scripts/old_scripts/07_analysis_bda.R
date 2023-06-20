source(here::here("scripts", "00_libs.R"))
source(here("scripts", "03_load_data.R"))

bda_coll = brm(correct_opt ~ L1*pre_post + (L1*pre_post | participant),
               family = "binomial", data = coll_tidy))

bda_sem = brm(correct_opt ~ L1*pre_post + (L1*pre_post | participant),
               family = "binomial", data = semantic_task,
              file = here("models", "bda_sem.rds"))

library("bayesplot")


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


mcmc_plot_function(bda_coll)
mcmc_plot_function(bda_sem)

