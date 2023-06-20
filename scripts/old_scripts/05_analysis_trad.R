source(here::here("scripts", "00_libs.R"))
source(here("scripts", "03_load_data.R"))

### correct ~ group*pre_or_post

res_aov <- aov(n_correct ~ L1*pre_post,
               data = desc_all)

summary(res_aov)


### Sample the amount of participants per group - make a function, 
### and run an anova

res_aov_2 <- aov(n_correct ~ L1*pre_post,
                 data = desc_all_2)


summary(res_aov_2)