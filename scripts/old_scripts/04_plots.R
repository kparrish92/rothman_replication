source(here::here("scripts", "00_libs.R"))
source(here("scripts", "03_load_data.R"))


## Context-based Collocation Task
ggplot(data=mean_correct, aes(x=L1, y=mean_c, fill=pre_post)) + 
  geom_bar(color = "black", position = 'dodge', stat='identity') +
  geom_text(aes(label=round(mean_c, digits = 2)), 
            position=position_dodge(width=0.9), vjust=-0.5, size = 3) +
  ylim(0,5) +
  theme_minimal() +
  ylab("Group mean (correct)") +
  theme(legend.position="bottom") +
  ggsave(here("docs", "figs", "cbc_desc.png"))

## Semantic interpretation task
ggplot(data=mean_correct_2, aes(x=L1, y=mean_c, fill=pre_post)) + 
  geom_bar(color = "black", position = 'dodge', stat='identity') +
  geom_text(aes(label=round(mean_c, digits = 2)), 
            position=position_dodge(width=0.9), vjust=-0.5, size = 3) +
  ylim(0,5) +
  theme_minimal() +
  ylab("Group mean (correct)") +
  theme(legend.position="bottom") +
  ggsave(here("docs", "figs", "semantic_desc.png"))


## Distribution number of correct responses per language
desc_all_2 %>% 
  ggplot(aes(x = n_correct, fill=pre_post)) + 
  geom_histogram(color = "black", position = 'dodge', binwidth = 1) +
  facet_grid(~L1)

## By token visualization
coll_tidy %>% 
  group_by(prompt, L1) %>% 
  summarize(n_correct = sum(correct_opt)) %>% 
  ggplot(aes(x = prompt, y = n_correct, color = L1)) +
  geom_point() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_hline(yintercept = 12, color = "red") +
  geom_hline(yintercept = 61, color = "green") +
  geom_hline(yintercept = 60, color = "blue")


semantic_task %>% 
  group_by(prompt, L1) %>% 
  summarize(n_correct = sum(correct_opt)) %>% 
  ggplot(aes(x = prompt, y = n_correct, color = L1)) +
  geom_point() + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  geom_hline(yintercept = 12, color = "red") +
  geom_hline(yintercept = 61, color = "green") +
  geom_hline(yintercept = 60, color = "blue")

## N participants 
part_df = desc_all %>% 
  group_by(L1) %>% 
  summarize(n = n()/2)

total = sum(part_df$n)

# Histogram of correct responses 
desc_all_2 %>% 
  ggplot(aes(x = n_correct, fill = L1)) + 
  geom_histogram(binwidth = 1, 
                 color = "black") +
  theme_minimal() + facet_wrap(~L1)

# Histogram of corråect responses 
desc_all %>% 
  ggplot(aes(x = n_correct, fill = L1)) + 
  geom_histogram(binwidth = 1, 
                 color = "black") +
  theme_minimal() + facet_wrap(~L1)


coll_tidy %>% 
  filter(L1 == "Portuguese") %>% 
  group_by(prompt, L1) %>% 
  summarise(n = n())


coll_tidy %>% 
  filter(L1 == "Portuguese") %>% 
  group_by(prompt) %>% 
  summarise(correct_no = sum(correct_opt)/61)


coll_tidy %>% 
  filter(L1 == "English") %>% 
  group_by(prompt, L1) %>% 
  summarise(n = n())

coll_tidy %>% 
  filter(L1 == "English") %>% 
  group_by(prompt) %>% 
  summarise(correct_no = sum(correct_opt)/12)

coll_tidy %>% 
  filter(L1 == "Spanish") %>% 
  group_by(prompt, L1) %>% 
  summarise(n = n())

coll_tidy %>% 
  filter(L1 == "Spanish") %>% 
  group_by(prompt) %>% 
  summarise(correct_no = sum(correct_opt)/60)


semantic_task %>% 
  filter(L1 == "Portuguese") %>% 
  group_by(prompt, L1) %>% 
  summarise(n = n())

semantic_task %>% 
  filter(L1 == "Portuguese") %>% 
  group_by(prompt) %>% 
  summarise(correct_no = sum(correct_opt)/60)


