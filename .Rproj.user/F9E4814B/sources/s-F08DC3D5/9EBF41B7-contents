source(here::here("scripts", "00_libs.R"))
source(here("scripts", "04_load_data.R"))

### aov collocation task 

col_mod_aov <- 
  afex::aov_ez(
    id = "gorilla_id", 
    dv = "n_correct", 
    between = "L1", 
    within = "pre_post", 
    data = aov_df_col, 
    print.formula = T
  )

df_aov_col = col_mod_aov[["anova_table"]] %>% 
  rename("num_df" = `num Df`) %>%
  rename("den_df" = `den Df`) %>% 
  rename("p.value" = `Pr(>F)`) 
  

round(df_aov_col$p.value[1], digits = 2)

### aov interpretation task 

int_mod_aov <- 
  afex::aov_ez(
    id = "gorilla_id", 
    dv = "n_correct", 
    between = "L1", 
    within = "pre_post", 
    data = aov_df_int, 
    print.formula = T
  )

df_aov_int = int_mod_aov[["anova_table"]] %>% 
  rename("num_df" = `num Df`) %>%
  rename("den_df" = `den Df`) %>% 
  rename("p.value" = `Pr(>F)`) 


### initial run - no main effect of group in both tasks. coll task had 
### a main effect for pre-post, and int had the interaction.

d_df = aov_df_col %>% 
  group_by(pre_post, L1) %>% 
  summarize(mean_corr = mean(n_correct),
            n = n(),
            sd_corr = sd(n_correct))

# TOST of col task in post
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

col_post_df = col_tost_post[["TOST"]]
col_post_df_eq = col_tost_post[["eqb"]]
col_post_df_es = col_tost_post[["effsize"]]

# TOST of col task in pre
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


col_pre_df = col_tost_pre[["TOST"]]
col_pre_df_eq = col_tost_pre[["eqb"]]
col_pre_df_es = col_tost_pre[["effsize"]]


d_df_i = aov_df_int %>% 
  group_by(pre_post, L1) %>% 
  summarize(mean_corr = mean(n_correct),
            n = n(),
            sd_corr = sd(n_correct))


# TOST of sem task in post
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


sem_post_df = sem_tost_post[["TOST"]]
sem_post_df_eq = sem_tost_post[["eqb"]]
sem_post_df_es = sem_tost_post[["effsize"]]



# TOST of sem task in pre
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

sem_pre_df = sem_tost_pre[["TOST"]]
sem_pre_df_eq = sem_tost_pre[["eqb"]]
sem_pre_df_eq = sem_tost_pre[["effsize"]]

round(mean(c(d_df_i$sd_corr, d_df$sd_corr)), digits = 1)


