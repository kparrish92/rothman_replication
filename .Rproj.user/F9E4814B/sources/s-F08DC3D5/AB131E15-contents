source(here::here("scripts", "00_libs.R"))
source(here("scripts", "04_load_data.R"))

glimpse(collocation_task)

collocation_task %>% 
  group_by(L1, pre_post, prolific_id) %>% 
  summarise(n = sum(is_correct)) %>% 
  group_by(L1) %>% 
  summarise(mean_correct = mean(n),
            sd_correct = sd(n),
            n = n())

TOSTtwo(m1 = 3.21, m2 = 3.26, sd1 = 1.33, sd2 = 1.12, 
               n1 = 56, n2 = 68, low_eqbound_d = -.4, high_eqbound_d = .4, alpha = .05,
               plot = TRUE, verbose = TRUE)

collocation_task %>% 
  group_by(L1, pre_post, prolific_id) %>% 
  summarise(n = sum(is_correct)) %>% 
  group_by(L1) %>% 
  summarise(mean_correct = mean(n),
            sd_correct = sd(n))
  
interpretation_task %>% 
  group_by(L1, pre_post, prolific_id) %>% 
  summarise(n = sum(is_correct)) %>% 
  group_by(L1) %>% 
  summarise(mean_correct = mean(n),
            sd_correct = sd(n))

  

desc_table = collocation_task %>% 
  group_by(L1) %>% 
  summarize(mean_p_prof = mean(lextale_score_port),
            mean_p_eng = mean(lextale_score_eng),
            mean_p_span = mean(lextale_score_span),
            mean_eng_aoa = mean(eng_aoa),
            mean_span_aoa = mean(span_aoa),
            mean_port_aoa = mean(port_aoa),
            n_total = length(unique(prolific_id)))

bda_coll = brm(is_correct | trials(1) ~ L1*pre_post + lextale_score_eng_std:L1 + lextale_score_port_std:L1 + 
                     lextale_score_span_std:L1 + (L1*pre_post | prolific_id),
                   family = "binomial", data = collocation_task, 
                   file = here("models", "bda_col_s.rds"))

conditional_effects(bda_coll)

bda_sem = brm(is_correct | trials(1) ~ L1*pre_post + lextale_score_eng_std:L1 + lextale_score_port_std:L1 + 
                      lextale_score_span_std:L1 + (L1*pre_post | prolific_id),
                    family = "binomial", data = interpretation_task, 
                    file = here("models", "bda_se._s.rds"))

conditional_effects(bda_sem)

mcmc_plot_function(bda_coll)
mcmc_plot_function(bda_sem)

