res = TOSTER::powerTOSTtwo(alpha=0.05, N=15, statistical_power=0.8)


collocation_task = read.csv(here("data_g", "tidy", "collocation_task.csv")) %>% 
  filter(L1 == "English" | L1 == "Spanish") %>%  
  mutate(lextale_score_eng_std = (lextale_score_eng - mean(lextale_score_eng)) / sd(lextale_score_eng),
         lextale_score_span_std = (lextale_score_span - mean(lextale_score_span)) / sd(lextale_score_span),
         lextale_score_port_std = (lextale_score_span - mean(lextale_score_span)) / sd(lextale_score_span))

interpretation_task = read.csv(here("data_g", "tidy", "interpretation_task.csv")) %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  filter(!is.na(L1)) %>% 
  mutate(lextale_score_eng_std = (lextale_score_eng - mean(lextale_score_eng)) / sd(lextale_score_eng),
         lextale_score_span_std = (lextale_score_span - mean(lextale_score_span)) / sd(lextale_score_span),
         lextale_score_port_std = (lextale_score_span - mean(lextale_score_span)) / sd(lextale_score_span))


missing_parts = read.csv(here("data_g", "tidy", "add_parts.csv"))

aov_df_col = collocation_task %>% 
  group_by(pre_post, gorilla_id, L1) %>% 
  summarize(correct_no_coll = sum(is_correct)) %>% 
  pivot_wider(names_from = "pre_post", values_from = "correct_no_coll") %>%
  rename("pre_coll" = "pre",
         "post_coll" = "post") %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  pivot_longer(cols = c("post_coll", "pre_coll"), names_to = "pre_post", values_to = "n_correct")

aov_df_int = interpretation_task %>% 
  group_by(pre_post, gorilla_id, L1) %>% 
  summarize(correct_no_coll = sum(is_correct)) %>% 
  pivot_wider(names_from = "pre_post", values_from = "correct_no_coll") %>%
  rename("pre_coll" = "pre",
         "post_coll" = "post") %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  pivot_longer(cols = c("post_coll", "pre_coll"), names_to = "pre_post", values_to = "n_correct")


collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarize(n = n())  %>% 
  group_by(L1) %>% 
  summarize(n = n())


tot_df = interpretation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarize(n = n())  %>% 
  group_by(L1) %>% 
  summarize(n = n())


aoa_df = collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarise(English = mean(eng_aoa), 
            Spanish = mean(span_aoa), 
            Portuguese = mean(port_aoa)) %>% 
  pivot_longer(cols = c("English", "Spanish", "Portuguese"), 
               names_to = "Language", 
               values_to = "Score")

rep_aoa = aoa_df %>% 
  group_by(L1, Language) %>% 
  summarise(mean_aoa = round(mean(Score), digits = 2), 
            sd_aoa = round(sd(Score), digits = 1))

## models 

col_bda = read_rds(here("models", "bda_col.rds"))

sem_bda = read_rds(here("models", "bda_sem.rds"))
