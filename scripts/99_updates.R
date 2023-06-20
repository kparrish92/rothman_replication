
source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))
source(here::here("scripts", "02_tidy_lextale.R"))
source(here::here("scripts", "03_tidy_tasks.R"))

source(here::here("scripts", "04_load_data.R"))

# verify updates 

collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarize(n = n())  %>% 
  group_by(L1) %>% 
  summarize(n = n())

interpretation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarize(n = n())  %>% 
  group_by(L1) %>% 
  summarize(n = n())