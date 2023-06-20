source(here::here("scripts", "00_libs.R"))
source(here("scripts", "04_load_data.R"))

# Plots 

# Proficiency in each language per group 

prof_df = collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarise(English = mean(lextale_score_eng), 
            Spanish = mean(lextale_score_span), 
            Portuguese = mean(lextale_score_port)) %>% 
  pivot_longer(cols = c("English", "Spanish", "Portuguese"), 
               names_to = "Language", 
               values_to = "Score")
  



prof_df %>% 
  ggplot(aes(y = Score, x = Language, fill = L1)) + 
  geom_boxplot() + theme_minimal() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  ggsave(here("docs", "figs", "prof_plot.png"))



## Context-based Collocation Task

desc_all = collocation_task %>% 
  group_by(prolific_id, pre_post, L1) %>% 
  summarize(n_correct = sum(is_correct))   

mean_correct = desc_all %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct),
            sd_c = sd(n_correct))

ggplot(data=mean_correct, aes(x=pre_post, y=mean_c, fill=L1)) + 
  geom_bar(color = "black", position = 'dodge', stat='identity') +
  geom_text(aes(label=paste0(round(mean_c, digits = 2), " (", 
                             round(sd_c, digits = 1), ")")), 
            position=position_dodge(width=0.9), vjust=-0.5, size = 3) +
  ylim(0,5) +
  theme_minimal() +
  ylab("Group mean (correct)") +
  theme(legend.position="bottom") +
  ggsave(here("docs", "figs", "cbc_desc.png"))

desc_all_i = interpretation_task %>% 
  group_by(prolific_id, pre_post, L1) %>% 
  summarize(n_correct = sum(is_correct))   

mean_correct_i = desc_all_i %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct),
            sd_c = sd(n_correct))


ggplot(data=mean_correct_i, aes(x=pre_post, y=mean_c, fill=L1)) + 
  geom_bar(color = "black", position = 'dodge', stat='identity') +
  geom_text(aes(label=paste0(round(mean_c, digits = 2), " (", 
                             round(sd_c, digits = 1), ")")), 
            position=position_dodge(width=0.9), vjust=-0.5, size = 3) +
  ylim(0,5) +
  theme_minimal() +
  ylab("Group mean (correct)") +
  theme(legend.position="bottom") +
  ggsave(here("docs", "figs", "semantic_desc.png"))


# plot models 

df_col_prof_eng = 
  conditional_effects(col_bda)[["lextale_score_eng:L1"]] %>% 
  mutate(language = "English") %>% 
  select(lextale_score_eng, L1, pre_post, estimate__, se__, language) %>% 
  rename("LexTALE score" = lextale_score_eng)
df_col_prof_span = 
  conditional_effects(col_bda)[["lextale_score_span:L1"]] %>% 
  mutate(language = "Spanish") %>% 
  select(lextale_score_span, L1, pre_post, estimate__, se__, language) %>% 
  rename("LexTALE score" = lextale_score_span)
df_col_prof_port = 
  conditional_effects(col_bda)[["lextale_score_port:L1"]] %>% 
  mutate(language = "Portuguese") %>% 
  select(lextale_score_port, L1, pre_post, estimate__, se__, language) %>% 
  rename("LexTALE score" = lextale_score_port)

all_col_df = rbind(df_col_prof_eng, df_col_prof_span, df_col_prof_port)

all_col_df %>% 
  ggplot(aes(x = `LexTALE score`, y = estimate__, color = language)) + 
  geom_ribbon(aes(ymin = estimate__ - se__, 
                  ymax = estimate__ + se__, fill = language), 
              alpha=0.2, 
              linetype = "blank") +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#b5b2b1")) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#b5b2b1")) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~L1) 
  
library(tidybayes)
library(bayesplot)
library(modelr)

col_df = collocation_task %>%
  data_grid(pre_post, L1, 
              lextale_score_port, 
              lextale_score_span,
              lextale_score_eng) %>% 
  add_fitted_draws(col_bda, dpar = FALSE, category = "is_correct | trials(1)",
                   re_formula = NA) 
