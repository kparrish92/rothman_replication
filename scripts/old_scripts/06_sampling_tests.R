source(here::here("scripts", "00_libs.R"))
source(here("scripts", "03_load_data.R"))

thisRun = 1
k = 100

hist(desc_all$n_correct)
hist(desc_all_2$n_correct)

df_f <- character()
for(thisRun in 1:k)
{
  df = desc_all_2
  #--------------------------------------------------------------#
  eng_bil = df %>% filter(L1 == "English")
  e_sub = sample_n(data.frame(p_list = 
                                unique(eng_bil$participant)), 12)
  eng_bil_s = eng_bil %>% 
    filter(participant %in% e_sub$p_list)
  #--------------------------------------------------------------#
  span_bil = df %>% filter(L1 == "Spanish")
  sp_sub = sample_n(data.frame(p_list = 
                                 unique(span_bil$participant)), 15)
  span_bil_s = span_bil  %>% 
    filter(participant %in% sp_sub$p_list)
  #--------------------------------------------------------------#
  span_l1 = df %>% filter(L1 == "Spanish monolingual")
  sl1_sub = sample_n(data.frame(p_list = 
                                  unique(span_l1$participant)), 16)
  span_l1_s = span_l1  %>% 
    filter(participant %in% sl1_sub$p_list)
  #--------------------------------------------------------------#
  port_l1 = df %>% filter(L1 == "Portuguese")
  port_sub = sample_n(data.frame(p_list = 
                                   unique(port_l1$participant)), 17)
  portl1_s = port_l1  %>% 
    filter(participant %in% port_sub$p_list)
  #--------------------------------------------------------------#
  
  a_df = rbind(eng_bil_s, span_bil_s, span_l1_s, portl1_s) 
  
  anova <- aov(n_correct ~ L1*pre_post,
               data = a_df)
  
  df_out = data.frame(group_main = summary(anova)[[1]][["Pr(>F)"]][1],
             condition = summary(anova)[[1]][["Pr(>F)"]][2],
             group_main_condition = summary(anova)[[1]][["Pr(>F)"]][3],
             trial = thisRun)

  df_f <- rbind(df_f, df_out)
}


sum(df_f$group_main < .05)
sum(df_f$condition < .05)
sum(df_f$group_main_condition < .05)
