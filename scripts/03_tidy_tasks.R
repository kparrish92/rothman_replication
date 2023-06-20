# Tidy experimental tasks --------------------------------------------------------------
#
# Last update: 2023-05-16
# This script loads and tidies the data for all 
# semeantic interpretation and collocation tasks 
#
# -----------------------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))

lextale_df = read.csv(here("data_g", "tidy", "lextale_df.csv"))

interpretation_task_ids = read.csv(here("data_g", "data_exp_128242-v3_questionnaire-9e8c.csv")) %>% 
  filter(Response.Type == "action") %>% 
  select("Participant.Private.ID",
         Response) %>%   
  rename("gorilla_id" = "Participant.Private.ID",
         "prolific_id" = "Response")

collocation_task_1 = read.csv(here("data_g", "data_exp_127819-v12_task-t5nj.csv")) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..prompt", 
         "Spreadsheet..correct",
         "Spreadsheet..opt_a",
         Response) %>%   
  rename("gorilla_id" = "Participant.Private.ID",
         "prompt" = "Spreadsheet..prompt",
         "opt_a" = "Spreadsheet..opt_a",
         "correct_answer" = "Spreadsheet..correct") %>% 
  mutate(is_correct = case_when(
    correct_answer == "opt_b" & Response == "B" ~ 1,
    correct_answer == "opt_b" & Response == "A" ~ 0,
    correct_answer == "opt_a" & Response == "A" ~ 1,
    correct_answer == "opt_a" & Response == "B" ~ 0,
  )) %>%  
  mutate(pre_post = case_when(
    opt_a == "pais orgulhosos" ~ "post",
    opt_a == "super-heróis corajosos" ~ "pre",
    opt_a == "opções várias" ~ "pre",
    opt_a == "Ming aventureiros" ~ "pre",
    opt_a == "pessoa mesma" ~ "pre",
    opt_a == "pais afetuosos" ~ "post",
    opt_a == "meninos fortes" ~ "post",
    opt_a == "amigos pobres" ~ "post",
    opt_a == "amigo velho" ~ "pre",
    opt_a == "estudantes estudiosos" ~ "post"
  )) %>% 
  filter(!is.na(pre_post)) 

# to make desc 
#%>% 
#  group_by(pre_post, gorilla_id) %>% 
#  summarize(correct_no_coll = sum(is_correct)) %>% 
#  pivot_wider(names_from = "pre_post", values_from = "correct_no_coll") %>%
#  rename("pre_coll" = "pre",
#        "post_coll" = "post")


collocation_task_2 = read.csv(here("data_g", "data_exp_127819-v11_task-t5nj.csv")) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..prompt", 
         "Spreadsheet..correct",
         "Spreadsheet..opt_a",
         Response) %>%   
  rename("gorilla_id" = "Participant.Private.ID",
         "prompt" = "Spreadsheet..prompt",
         "opt_a" = "Spreadsheet..opt_a",
         "correct_answer" = "Spreadsheet..correct") %>% 
  mutate(is_correct = case_when(
    correct_answer == "opt_b" & Response == "B" ~ 1,
    correct_answer == "opt_b" & Response == "A" ~ 0,
    correct_answer == "opt_a" & Response == "A" ~ 1,
    correct_answer == "opt_a" & Response == "B" ~ 0,
  )) %>%  
  mutate(pre_post = case_when(
    opt_a == "pais orgulhosos" ~ "post",
    opt_a == "super-heróis corajosos" ~ "pre",
    opt_a == "opções várias" ~ "pre",
    opt_a == "Ming aventureiros" ~ "pre",
    opt_a == "pessoa mesma" ~ "pre",
    opt_a == "pais afetuosos" ~ "post",
    opt_a == "meninos fortes" ~ "post",
    opt_a == "amigos pobres" ~ "post",
    opt_a == "amigo velho" ~ "pre",
    opt_a == "estudantes estudiosos" ~ "post"
  )) %>% 
  filter(!is.na(pre_post)) 

collocation_task = rbind(collocation_task_1, collocation_task_2) %>%
  left_join(lextale_df, by = "gorilla_id")  %>% 
  write.csv(here("data_g", "tidy", "collocation_task.csv"))

interpretation_task_1 = read.csv(here("data_g", "data_exp_127819-v12_task-6yod.csv")) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..prompt", 
         "Spreadsheet..correct",
         "Spreadsheet..opt_b",
         Response) %>%   
  rename("gorilla_id" = "Participant.Private.ID",
         "prompt" = "Spreadsheet..prompt",
         "opt_b" = "Spreadsheet..opt_b",
         "correct_answer" = "Spreadsheet..correct") %>% 
  mutate(is_correct = case_when(
    correct_answer == "opt_b" & Response == "B" ~ 1,
    correct_answer == "opt_b" & Response == "A" ~ 0,
    correct_answer == "opt_a" & Response == "A" ~ 1,
    correct_answer == "opt_a" & Response == "B" ~ 0,
  )) %>% 
  mutate(pre_post = case_when(
    opt_b == "Possui dois Lumpears na sala de estar que as relíquias." ~ "post",     
    opt_b == "Os vizinhos não têm dinheiro." ~ "pre",                               
    opt_b == "Eles falam um idioma que muito poucas pessoas falam." ~ "pre",                         
    opt_b == "O homem se sente sozinho." ~ "post",                                   
    opt_b == "Comprei outro computador." ~ "pre",                                   
    opt_b == "Ele contém informações que são para verar." ~ "post",                  
    opt_b == "O presidente que já havia falado com a classe." ~ "pre",              
    opt_b == "Meu amigo não é jovem." ~ "pre", 
    opt_b == "Mariela é velha." ~ "pre",
    opt_b == "Não há ninguém (tão especial) quanto Marta, que eu te disse." ~ "post",
    opt_b == "Os trabalhos são ótimos." ~ "post",
    opt_b == "A pintura é enorme e custa muito dinheiro" ~ "post",
    opt_b == "Não contei a você sobre mais ninguém, além de Marta." ~ "post")) %>% 
  filter(!is.na(pre_post)) %>% 
  left_join(lextale_df, by = "gorilla_id") 


int_2_temp = read.csv(here("data_g", "data_exp_128242-v3_task-6yod.csv")) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..prompt", 
         "Spreadsheet..correct",
         "Spreadsheet..opt_b",
         Response) %>%   
  rename("gorilla_id" = "Participant.Private.ID",
         "prompt" = "Spreadsheet..prompt",
         "opt_b" = "Spreadsheet..opt_b",
         "correct_answer" = "Spreadsheet..correct") %>% 
  mutate(is_correct = case_when(
    correct_answer == "opt_b" & Response == "B" ~ 1,
    correct_answer == "opt_b" & Response == "A" ~ 0,
    correct_answer == "opt_a" & Response == "A" ~ 1,
    correct_answer == "opt_a" & Response == "B" ~ 0,
  )) %>% 
  mutate(pre_post = case_when(
    opt_b == "Possui dois Lumpears na sala de estar que as relíquias." ~ "post",     
    opt_b == "Os vizinhos não têm dinheiro." ~ "pre",                               
    opt_b == "Eles falam um idioma que muito poucas pessoas falam." ~ "pre",                         
    opt_b == "O homem se sente sozinho." ~ "post",                                   
    opt_b == "Comprei outro computador." ~ "pre",                                   
    opt_b == "Ele contém informações que são para verar." ~ "post",                  
    opt_b == "O presidente que já havia falado com a classe." ~ "pre",              
    opt_b == "Meu amigo não é jovem." ~ "pre", 
    opt_b == "Mariela é velha." ~ "pre",
    opt_b == "Não há ninguém (tão especial) quanto Marta, que eu te disse." ~ "post",
    opt_b == "Os trabalhos são ótimos." ~ "post",
    opt_b == "Eles falam uma linguagem estranha." ~ "pre",
    opt_b == "A pintura é enorme e custa muito dinheiro" ~ "post",
    opt_b == "Não contei a você sobre mais ninguém, além de Marta." ~ "post")) %>% 
  filter(!is.na(pre_post)) 


tt = left_join(lextale_df, interpretation_task_ids, by = "prolific_id") %>% 
  rename("gorilla_id_o" = "gorilla_id.x") %>% 
  rename("gorilla_id" = "gorilla_id.y") %>% 
  filter(!is.na(gorilla_id))

interpretation_task_2 = int_2_temp %>% 
  left_join(tt, by = "gorilla_id") %>% 
  select(-gorilla_id_o)


interpretation_task = rbind(interpretation_task_1, interpretation_task_2) %>% 
  write.csv(here("data_g", "tidy", "interpretation_task.csv"))

