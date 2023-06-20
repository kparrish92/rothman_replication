
coll_tidy = 
  read.csv(here("data", "tidy", "collocation_tidy.csv"))

semantic_task =
  read.csv(here("data", "tidy", "semantic_tidy.csv"))

desc_all = coll_tidy %>% 
  group_by(participant, pre_post, L1) %>% 
  summarize(n_correct = sum(correct_opt)) 

desc_all_2 = semantic_task %>% 
  group_by(participant, pre_post, L1) %>% 
  summarize(n_correct = sum(correct_opt)) 

mean_correct = desc_all %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct))

desc_all %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct),
            sd_c = sd(n_correct))


mean_correct_2 = desc_all_2 %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct))


bda_coll = readRDS(here("models", "bda_coll.rds"))

fixef_bda_coll = summary(bda_coll)