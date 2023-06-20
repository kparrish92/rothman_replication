source(here("scripts", "03_load_data.R"))

## N participants 
part_df = desc_all %>% 
  group_by(L1) %>% 
  summarize(n = n()/2)

sum(part_df$n)