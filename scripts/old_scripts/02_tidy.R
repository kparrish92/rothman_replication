source(here::here("scripts", "00_libs.R"))
source(here("scripts", "00_b_tidy_screening.R"))
source(here("scripts", "01_helpers.R"))



tidy_task1_span_m <- dir_ls(here("data", "raw", "spanish_l1_mono"),
                         regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(choice.keys)) %>% 
  filter(!is.na(participant)) %>%
  select(participant, choice.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    choice.keys == 1 & correct == "opt_a" ~ 1,
    choice.keys == 0 & correct == "opt_b" ~ 1,
    choice.keys == 1 & correct == "opt_b" ~ 0,
    choice.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "padres orgullosos" ~ "post",
    opt_a == "super-héroes valientes" ~ "pre",
    opt_a == "opciones varias" ~ "pre",
    opt_a == "libro nuevo" ~ "pre",
    opt_a == "persona misma" ~ "pre",
    opt_a == "padres cariñosos" ~ "post",
    opt_a == "chicos fuertes" ~ "post",
    opt_a == "amigos pobres" ~ "post",
    opt_a == "amiga vieja" ~ "pre",
    opt_a == "estudiantes estudiosos" ~ "post"
  )) %>% 
  mutate(L1 = "Spanish monolingual")
  

tidy_task1_eng <- dir_ls(here("data", "raw"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(participant %in% eng_l1_ids$prolific_id) %>% 
  filter(!is.na(choice.keys)) %>% 
  filter(!is.na(participant)) %>%
  select(participant, choice.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    choice.keys == 1 & correct == "opt_a" ~ 1,
    choice.keys == 0 & correct == "opt_b" ~ 1,
    choice.keys == 1 & correct == "opt_b" ~ 0,
    choice.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "pais orgulhosos" ~ "post",
    opt_a == "super-heróis valentes" ~ "pre",
    opt_a == "opções várias" ~ "pre",
    opt_a == "Livro novo" ~ "pre",
    opt_a == "pessoa mesma" ~ "pre",
    opt_a == "pais afetuosos" ~ "post",
    opt_a == "meninos fortes" ~ "post",
    opt_a == "amigos pobres" ~ "post",
    opt_a == "amigo velho" ~ "pre",
    opt_a == "estudantes estudiosos" ~ "post"
  )) %>% 
  mutate(L1 = "English")

tidy_task1_span <- dir_ls(here("data", "raw"),
                         regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(participant %in% span_l1_ids$prolific_id) %>% 
  filter(!is.na(choice.keys)) %>% 
  filter(!is.na(participant)) %>%
  select(participant, choice.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    choice.keys == 1 & correct == "opt_a" ~ 1,
    choice.keys == 0 & correct == "opt_b" ~ 1,
    choice.keys == 1 & correct == "opt_b" ~ 0,
    choice.keys == 0 & correct == "opt_a" ~ 0)) %>% 
  mutate(pre_post = case_when(
    opt_a == "pais orgulhosos" ~ "post",
    opt_a == "super-heróis valentes" ~ "pre",
    opt_a == "opções várias" ~ "pre",
    opt_a == "Livro novo" ~ "pre",
    opt_a == "pessoa mesma" ~ "pre",
    opt_a == "pais afetuosos" ~ "post",
    opt_a == "meninos fortes" ~ "post",
    opt_a == "amigos pobres" ~ "post",
    opt_a == "amigo velho" ~ "pre",
    opt_a == "estudantes estudiosos" ~ "post")) %>% 
  mutate(L1 = "Spanish")


tidy_task1_port <- dir_ls(here("data", "raw"),
                         regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(participant %in% port_speakers$Participant.id) %>% 
  filter(!is.na(choice.keys)) %>% 
  filter(!is.na(participant)) %>%
  select(participant, choice.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    choice.keys == 1 & correct == "opt_a" ~ 1,
    choice.keys == 0 & correct == "opt_b" ~ 1,
    choice.keys == 1 & correct == "opt_b" ~ 0,
    choice.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "pais orgulhosos" ~ "post",
    opt_a == "super-heróis valentes" ~ "pre",
    opt_a == "opções várias" ~ "pre",
    opt_a == "Livro novo" ~ "pre",
    opt_a == "pessoa mesma" ~ "pre",
    opt_a == "pais afetuosos" ~ "post",
    opt_a == "meninos fortes" ~ "post",
    opt_a == "amigos pobres" ~ "post",
    opt_a == "amigo velho" ~ "pre",
    opt_a == "estudantes estudiosos" ~ "post"
  )) %>% 
  mutate(L1 = "Portuguese")

task1_tidy = rbind(tidy_task1_port, 
                   tidy_task1_eng, 
                   tidy_task1_span,
                   tidy_task1_span_m)

task1_tidy %>% 
  write.csv(here("data", "tidy", "collocation_tidy.csv"))

##### Semantic task 


tidy_task2_span_m <- dir_ls(here("data", "raw", "spanish_l1_mono"),
                            regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_3.keys)) %>% 
  filter(!is.na(participant)) %>% 
  select(participant, key_resp_3.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    key_resp_3.keys == 1 & correct == "opt_a" ~ 1,
    key_resp_3.keys == 0 & correct == "opt_b" ~ 1,
    key_resp_3.keys == 1 & correct == "opt_b" ~ 0,
    key_resp_3.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "Tiene dos lámparas viejas en el salón." ~ "post",
    opt_a == "El presidente, y no otra persona, habló con las clase." ~ "pre",
    opt_a == "El cuadro es impresionante y cuesta mucho dinero" ~ "post",
    opt_a == "Mi amigo ha sido mi amigo por muchos años" ~ "post",
    opt_a == "Hace muchos años que son amigas." ~ "pre",
    opt_a == "La computadora era 100% nueva." ~ "pre",
    opt_a == "No te hablé de nadie más, aparte de Marta." ~ "post",
    opt_a == "Las obras no son pequeñas." ~ "pre",
    opt_a == "Hablan una lengua que muy poca gente habla." ~ "pre",
    opt_a == "Solo vive un hombre en la casa." ~ "post"
  )) %>% 
  mutate(L1 = "Spanish monolingual")

tidy_task2_eng <- dir_ls(here("data", "raw"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(participant %in% eng_l1_ids$prolific_id) %>% 
  filter(!is.na(key_resp_3.keys)) %>% 
  filter(!is.na(participant)) %>% 
  filter(participant != "test") %>% 
  filter(participant != "ex") %>% 
  select(participant, key_resp_3.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    key_resp_3.keys == 1 & correct == "opt_a" ~ 1,
    key_resp_3.keys == 0 & correct == "opt_b" ~ 1,
    key_resp_3.keys == 1 & correct == "opt_b" ~ 0,
    key_resp_3.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "Ele tem duas lâmpadas velhas na sala." ~ "post",
    opt_a == "O presidente, e não outra pessoa, conversou com a classe." ~ "pre",
    opt_a == "A pintura é impressionante e custa muito dinheiro" ~ "post",
    opt_a == "Meu amigo é meu amigo há muitos anos" ~ "post",
    opt_a == "Muitos anos atrás eles são amigos." ~ "pre",
    opt_a == "O computador era 100% novo." ~ "pre",
    opt_a == "Não há ninguém (tão especial) quanto Marta, que eu te disse." ~ "post",
    opt_a == "Os trabalhos não são pequenos." ~ "pre",
    opt_a == "Eles falam uma linguagem estranha." ~ "pre",
    opt_a == "Apenas um homem mora em casa." ~ "post"
  )) %>% 
  mutate(L1 = "English")


tidy_task2_span <- dir_ls(here("data", "raw"),
                         regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(participant %in% span_l1_ids$prolific_id) %>% 
  filter(!is.na(key_resp_3.keys)) %>% 
  filter(!is.na(participant)) %>% 
  filter(participant != "test") %>% 
  filter(participant != "ex") %>% 
  select(participant, key_resp_3.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    key_resp_3.keys == 1 & correct == "opt_a" ~ 1,
    key_resp_3.keys == 0 & correct == "opt_b" ~ 1,
    key_resp_3.keys == 1 & correct == "opt_b" ~ 0,
    key_resp_3.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "Ele tem duas lâmpadas velhas na sala." ~ "post",
    opt_a == "O presidente, e não outra pessoa, conversou com a classe." ~ "pre",
    opt_a == "A pintura é impressionante e custa muito dinheiro" ~ "post",
    opt_a == "Meu amigo é meu amigo há muitos anos" ~ "post",
    opt_a == "Muitos anos atrás eles são amigos." ~ "pre",
    opt_a == "O computador era 100% novo." ~ "pre",
    opt_a == "Não há ninguém (tão especial) quanto Marta, que eu te disse." ~ "post",
    opt_a == "Os trabalhos não são pequenos." ~ "pre",
    opt_a == "Eles falam uma linguagem estranha." ~ "pre",
    opt_a == "Apenas um homem mora em casa." ~ "post"
  )) %>% 
  mutate(L1 = "Spanish")

tidy_task2_port <- dir_ls(here("data", "raw"),
                         regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(participant %in% port_speakers$Participant.id) %>% 
  filter(!is.na(key_resp_3.keys)) %>% 
  filter(!is.na(participant)) %>% 
  filter(participant != "test") %>% 
  filter(participant != "ex") %>% 
  select(participant, key_resp_3.keys, prompt, correct, opt_a) %>% 
  mutate(correct_opt = case_when(
    key_resp_3.keys == 1 & correct == "opt_a" ~ 1,
    key_resp_3.keys == 0 & correct == "opt_b" ~ 1,
    key_resp_3.keys == 1 & correct == "opt_b" ~ 0,
    key_resp_3.keys == 0 & correct == "opt_a" ~ 0
  )) %>% 
  mutate(pre_post = case_when(
    opt_a == "Ele tem duas lâmpadas velhas na sala." ~ "post",
    opt_a == "O presidente, e não outra pessoa, conversou com a classe." ~ "pre",
    opt_a == "A pintura é impressionante e custa muito dinheiro" ~ "post",
    opt_a == "Meu amigo é meu amigo há muitos anos" ~ "post",
    opt_a == "Muitos anos atrás eles são amigos." ~ "pre",
    opt_a == "O computador era 100% novo." ~ "pre",
    opt_a == "Não há ninguém (tão especial) quanto Marta, que eu te disse." ~ "post",
    opt_a == "Os trabalhos não são pequenos." ~ "pre",
    opt_a == "Eles falam uma linguagem estranha." ~ "pre",
    opt_a == "Apenas um homem mora em casa." ~ "post"
  )) %>% 
  mutate(L1 = "Portuguese")

semantic_task = rbind(tidy_task2_eng, tidy_task2_port, 
                      tidy_task2_span, tidy_task2_span_m)

semantic_task %>% 
  write.csv(here("data", "tidy", "semantic_tidy.csv"))

