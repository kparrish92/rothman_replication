# ------------------------------------------------------
# Date 11/9/23
# This script runs the analyses reported in the manuscript
# ANOVA, TOST
# -------------------------------------------------------

## Load Libraries and data.
source(here::here("scripts", "00_libs.R"))
source(here("scripts", "04_load_data.R"))



## ANOVA for the collocation task 
col_mod_aov <- 
  afex::aov_ez(
    id = "gorilla_id", 
    dv = "n_correct", 
    between = "L1", 
    within = "pre_post", 
    data = aov_df_col, 
    print.formula = T
  )


## Assumptions 
performance::check_homogeneity(col_mod_aov) # homogeneity 
performance::check_sphericity(col_mod_aov) # sphericity  
p = performance::check_normality(col_mod_aov) # check normality


plot(p)

## Extract data for reporting 
df_aov_col = col_mod_aov[["anova_table"]] %>% 
  rename("num_df" = `num Df`) %>%
  rename("den_df" = `den Df`) %>% 
  rename("p.value" = `Pr(>F)`) 
  

## ANOVA for the interpretation task 
int_mod_aov <- 
  afex::aov_ez(
    id = "gorilla_id", 
    dv = "n_correct", 
    between = "L1", 
    within = "pre_post", 
    data = aov_df_int, 
    print.formula = T
  )

## Assumptions 
performance::check_homogeneity(int_mod_aov) # homogeneity 
performance::check_sphericity(int_mod_aov) # sphericity  
m = performance::check_normality(int_mod_aov) # check normality


plot(m)

## Extract data for reporting 
df_aov_int = int_mod_aov[["anova_table"]] %>% 
  rename("num_df" = `num Df`) %>%
  rename("den_df" = `den Df`) %>% 
  rename("p.value" = `Pr(>F)`) 


## Extract data for TOST
d_df = aov_df_col %>% 
  group_by(pre_post, L1) %>% 
  summarize(mean_corr = mean(n_correct),
            n = n(),
            sd_corr = sd(n_correct))

## TOST of collocation task in the POST condition
col_tost_post = TOSTER::tsum_TOST(m1 = d_df$mean_corr[1],
                  sd1 = d_df$sd_corr[1],
                  n1 = d_df$n[1],
                  m2 = d_df$mean_corr[2],
                  sd2 = d_df$sd_corr[2], 
                  n2 = d_df$n[2],
                  low_eqbound = -.4,
                  high_eqbound = .4,
                  alpha = .05,
                  eqbound_type = c("SMD"))

## Extract data for reporting 
col_post_df = col_tost_post[["TOST"]]
col_post_df_eq = col_tost_post[["eqb"]]
col_post_df_es = col_tost_post[["effsize"]]

## TOST of collocation task in the PRE condition
col_tost_pre = TOSTER::tsum_TOST(m1 = d_df$mean_corr[3],
                                  sd1 = d_df$sd_corr[3],
                                  n1 = d_df$n[3],
                                  m2 = d_df$mean_corr[4],
                                  sd2 = d_df$sd_corr[4], 
                                  n2 = d_df$n[4],
                                  low_eqbound = -.4,
                                  high_eqbound = .4,
                                  alpha = .05,
                                  eqbound_type = c("SMD"))

## Extract data for reporting 
col_pre_df = col_tost_pre[["TOST"]]
col_pre_df_eq = col_tost_pre[["eqb"]]
col_pre_df_es = col_tost_pre[["effsize"]]

## Extract data for TOST
d_df_i = aov_df_int %>% 
  group_by(pre_post, L1) %>% 
  summarize(mean_corr = mean(n_correct),
            n = n(),
            sd_corr = sd(n_correct))

## TOST of interpretation task in the POST condition
sem_tost_post = TOSTER::tsum_TOST(m1 = d_df_i$mean_corr[1],
                                  sd1 = d_df_i$sd_corr[1],
                                  n1 = d_df_i$n[1],
                                  m2 = d_df_i$mean_corr[2],
                                  sd2 = d_df_i$sd_corr[2], 
                                  n2 = d_df_i$n[2],
                                  low_eqbound = -.4,
                                  high_eqbound = .4,
                                  alpha = .05,
                                  eqbound_type = c("SMD"))

## Extract data for reporting 
sem_post_df = sem_tost_post[["TOST"]]
sem_post_df_eq = sem_tost_post[["eqb"]]
sem_post_df_es = sem_tost_post[["effsize"]]


## TOST of interpretation task in the PRE condition
sem_tost_pre = TOSTER::tsum_TOST(m1 = d_df_i$mean_corr[3],
                                 sd1 = d_df_i$sd_corr[3],
                                 n1 = d_df_i$n[3],
                                 m2 = d_df_i$mean_corr[4],
                                 sd2 = d_df_i$sd_corr[4], 
                                 n2 = d_df_i$n[4],
                                 low_eqbound = -.4,
                                 high_eqbound = .4,
                                 alpha = .05,
                                 eqbound_type = c("SMD"))
## Extract data for reporting 
sem_pre_df = sem_tost_pre[["TOST"]]
sem_pre_df_eq = sem_tost_pre[["eqb"]]
sem_pre_df_eq = sem_tost_pre[["effsize"]]



