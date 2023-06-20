

## tidy eng lex 

eng_lextale = read.csv(here("data_g", "data_exp_127819-v12_task-fg99.csv"),
                       na.strings = c("")
                       ) %>% 
  select("Participant.Private.ID",
"Spreadsheet..targetresponse",
"Spreadsheet..item",
"Spreadsheet..type",
Correct, Response) %>% 
  rename("gorilla_id" = "Participant.Private.ID",
         "correct_answer" = "Spreadsheet..targetresponse",
         "item" = "Spreadsheet..item",
         "type" = "Spreadsheet..type") %>% 
  filter(!is.na(type)) %>% 
  filter(type != "filler") %>% 
  mutate(Response = case_when(
    Response == "tick.png" ~ "word",
    Response == "cross.png" ~ "non"
  )) %>% 
  group_by(type, gorilla_id) %>% 
  summarize(correct_n = sum(Correct)) %>% 
  pivot_wider(names_from = "type", values_from = "correct_n") %>% 
  mutate(lextale_score_eng = 
           ((word/40*100) + (non/20*100)) / 2)
 
eng_lextale_2 = read.csv(here("data_g", "data_exp_127819-v1_task-fg99.csv"),
                       na.strings = c("")
) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..targetresponse",
         "Spreadsheet..item",
         "Spreadsheet..type",
         Correct, Response) %>% 
  rename("gorilla_id" = "Participant.Private.ID",
         "correct_answer" = "Spreadsheet..targetresponse",
         "item" = "Spreadsheet..item",
         "type" = "Spreadsheet..type") %>% 
  filter(!is.na(type)) %>% 
  filter(type != "filler") %>% 
  mutate(Response = case_when(
    Response == "tick.png" ~ "word",
    Response == "cross.png" ~ "non"
  )) %>% 
  group_by(type, gorilla_id) %>% 
  summarize(correct_n = sum(Correct)) %>% 
  pivot_wider(names_from = "type", values_from = "correct_n") %>% 
  mutate(lextale_score_eng = 
           ((word/40*100) + (non/20*100)) / 2)

port_lextale = read.csv(here("data_g", "data_exp_127819-v12_task-gdok.csv"),
                        na.strings = c("")) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..targetresponse",
         "Spreadsheet..item",
         "Spreadsheet..type",
         Correct, Response) %>% 
  rename("gorilla_id" = "Participant.Private.ID",
         "correct_answer" = "Spreadsheet..targetresponse",
         "item" = "Spreadsheet..item",
         "type" = "Spreadsheet..type") %>% 
  filter(!is.na(type)) %>% 
  filter(type != "filler") %>% 
  mutate(Response = case_when(
    Response == "tick.png" ~ "word",
    Response == "cross.png" ~ "non"
  )) %>% 
  group_by(type, gorilla_id) %>% 
  summarize(correct_n = sum(Correct)) %>% 
  pivot_wider(names_from = "type", values_from = "correct_n") %>% 
  mutate(lextale_score_port = 
           ((word/60*100) + (non/30*100)) / 2)


span_lextale = read.csv(here("data_g", "data_exp_127819-v12_task-6d1f.csv"),
                        na.strings = c("")) %>% 
  select("Participant.Private.ID",
         "Spreadsheet..targetresponse",
         "Spreadsheet..item",
         "Spreadsheet..type",
         Correct, Response) %>% 
  rename("gorilla_id" = "Participant.Private.ID",
         "correct_answer" = "Spreadsheet..targetresponse",
         "item" = "Spreadsheet..item",
         "type" = "Spreadsheet..type") %>% 
  filter(!is.na(type)) %>% 
  filter(type != "filler") %>% 
  mutate(Response = case_when(
    Response == "tick.png" ~ "word",
    Response == "cross.png" ~ "non"
  )) %>% 
  group_by(type, gorilla_id) %>% 
  summarize(correct_n = sum(Correct)) %>% 
  pivot_wider(names_from = "type", values_from = "correct_n") %>% 
  mutate(lextale_score_span = 
           ((word/60*100) + (non/30*100)) / 2)


questionnaire = read.csv(here("data_g", "data_exp_127819-v12_questionnaire-zbjj.csv"),
                         na.strings = c("")) %>% 
  select(Participant.Private.ID, Question, Response, Response.Type,
         Key) %>% 
  filter(!is.na(Question)) %>% 
  filter(Key =="value") %>% 
  filter(Response.Type == "response") %>% 
  pivot_wider(names_from = Question, values_from = Response)
  

names(questionnaire)[1] = "gorilla_id"
names(questionnaire)[4] = "L1"
names(questionnaire)[5] = "yes_to_include"
names(questionnaire)[6] = "eng_aoa"
names(questionnaire)[7] = "span_aoa"
names(questionnaire)[8] = "port_aoa"
names(questionnaire)[9] = "prolific_id"

collocation_task = read.csv(here("data_g", "data_exp_127819-v12_task-t5nj.csv")) %>% 
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
  filter(!is.na(pre_post)) %>% 
  group_by(pre_post, gorilla_id) %>% 
  summarize(correct_no_coll = sum(is_correct)) %>% 
  pivot_wider(names_from = "pre_post", values_from = "correct_no_coll") %>%
  rename("pre_coll" = "pre",
         "post_coll" = "post")



interpretation_task = read.csv(here("data_g", "data_exp_127819-v12_task-6yod.csv")) %>% 
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
  group_by(pre_post, gorilla_id) %>% 
  summarize(correct_no_int = sum(is_correct)) %>% 
  pivot_wider(names_from = "pre_post", values_from = "correct_no_int") %>% 
  rename("pre_int" = "pre",
         "post_int" = "post")

final_df = left_join(questionnaire, 
                     eng_lextale, by = "gorilla_id") %>% 
  left_join(port_lextale, by = "gorilla_id") %>%
  left_join(span_lextale, by = "gorilla_id") %>% 
  left_join(collocation_task, by = "gorilla_id") %>% 
  left_join(interpretation_task, by = "gorilla_id") %>% 
  select(gorilla_id, L1, yes_to_include, eng_aoa,
         span_aoa, port_aoa, prolific_id, lextale_score_eng,
         lextale_score_port, lextale_score_span, pre_int, post_int,
         pre_coll, post_coll)


mean(as.numeric(final_df$eng_aoa))
mean(as.numeric(final_df$span_aoa))
mean(as.numeric(final_df$port_aoa))


mean(as.numeric(final_df$lextale_score_eng))
mean(as.numeric(final_df$lextale_score_span))
mean(as.numeric(final_df$lextale_score_port))


mean(as.numeric(final_df$pre_int))
mean(as.numeric(final_df$post_int))
mean(as.numeric(final_df$pre_coll))
mean(as.numeric(final_df$post_coll))


inc_df = final_df %>% 
  filter(L1 == "English" | L1 == "Spanish")


prof_df = inc_df %>%
  select(prolific_id, lextale_score_eng, lextale_score_span, 
         lextale_score_port, L1) %>% 
  pivot_longer(cols = c(lextale_score_eng, lextale_score_span, 
                        lextale_score_port), 
               names_to = "test", values_to = "score")

prof_df %>% 
  ggplot(aes(x = score, y = test)) + geom_point()


prof_df %>% 
  ggplot(aes(x = score, y = test, fill = L1)) + geom_boxplot()



final_df %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  select(prolific_id, pre_int, post_int, L1) %>% 
  pivot_longer(cols = c(pre_int, post_int), 
               names_to = "test", values_to = "score") %>% 
  group_by(L1) %>% 
  summarize(mean_score = mean(as.numeric(score)),
            sd_score = sd(score))
  

final_df %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  select(prolific_id, pre_coll, post_coll, L1) %>% 
  pivot_longer(cols = c(pre_coll, post_coll), 
               names_to = "test", values_to = "score") %>% 
  group_by(L1) %>% 
  summarize(mean_score = mean(as.numeric(score)),
            sd_score = sd(score))


all = rbind(add_parts, final_df) 

# How many per group

all %>% 
  filter(!is.na(pre_int)) %>% 
  group_by(L1) %>% 
  summarize(n = n())
 
all %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  select(prolific_id, pre_coll, post_coll, L1) %>% 
  pivot_longer(cols = c(pre_coll, post_coll), 
               names_to = "test", values_to = "score") %>% 
  group_by(L1) %>% 
  summarize(mean_score = mean(as.numeric(score)),
            sd_score = sd(score))


all %>% 
  filter(!is.na(pre_int)) %>% 
  filter(L1 == "English" | L1 == "Spanish") %>% 
  select(prolific_id, pre_int, post_int, L1) %>% 
  pivot_longer(cols = c(pre_int, post_int), 
               names_to = "test", values_to = "score") %>% 
  group_by(L1) %>% 
  summarize(mean_score = mean(as.numeric(score)),
            sd_score = sd(score))

all %>% 
  group_by(L1) %>% 
  summarize(n = n())


effect_size(3.24, 3.35, 1.39, 1.1)


effect_size(2.46, 3.01, 1.09, 1.19)

