# ------------------------------------------------------
# Date 11/9/23
# This script loads and tidies the data for all three lextale tasks
# 
# Note: Each Lextale (in Spanish, Portuguese and English) has 2 data files, 
# which had minor mismatches in the structure of the raw data. As a result, 
# these files are individually loaded, tidied, and subsequently combined. The 
# questionnaire data is also tidied here and combined with the lextale results
# -------------------------------------------------------


## Load Libraries and Helper functions
source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))

## Tidy the English Lextale (part 1)
eng_lextale_1 = read.csv(here("data", "lextale", "data_exp_127819-v12_task-fg99.csv"),
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



## Tidy the English Lextale (part 2)
eng_lextale_2 = read.csv(here("data", "lextale", "data_exp_127819-v11_task-fg99.csv"),
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

## Combine both English Lextale files
eng_lextale = rbind(eng_lextale_1, eng_lextale_2)

## Tidy Portuguese Lextale (part 1)
port_lextale_1 = read.csv(here("data", "lextale", "data_exp_127819-v12_task-gdok.csv"),
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

# Tidy Portuguese Lextale (part 2)
port_lextale_2 = read.csv(here("data", "lextale", "data_exp_127819-v11_task-gdok.csv"),
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


## Combine Portuguese Lextale
port_lextale = rbind(port_lextale_1, port_lextale_2)

$# Tidy Spanish lextale (part 1)
span_lextale_1 = read.csv(here("data", "lextale", "data_exp_127819-v12_task-6d1f.csv"),
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

## Tidy Spanish Lextale (part 2)
span_lextale_2 = read.csv(here("data", "lextale", "data_exp_127819-v11_task-6d1f.csv"),
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

## Combine Spanish Lextale 
span_lextale = rbind(span_lextale_1, span_lextale_2)

## Tidy questionnaire data to add to Lextale data (part 1)
questionnaire_1 = read.csv(here("data", "lextale", "data_exp_127819-v12_questionnaire-zbjj.csv"),
                         na.strings = c("")) %>% 
  select(Participant.Private.ID, Question, Response, Response.Type,
         Key) %>% 
  filter(!is.na(Question)) %>% 
  filter(Key =="value") %>% 
  filter(Response.Type == "response") %>% 
  pivot_wider(names_from = Question, values_from = Response)

## Tidy questionnaire data to add to Lextale data (part 2)
questionnaire_2 = read.csv(here("data", "lextale", "data_exp_127819-v11_questionnaire-zbjj.csv"),
                         na.strings = c("")) %>% 
  select(Participant.Private.ID, Question, Response, Response.Type,
         Key) %>% 
  filter(!is.na(Question)) %>% 
  filter(Key =="value") %>% 
  filter(Response.Type == "response") %>% 
  pivot_wider(names_from = Question, values_from = Response)

## Combine questionnaire versions
questionnaire = rbind(questionnaire_1, questionnaire_2)

## Rename columns 
names(questionnaire)[1] = "gorilla_id"
names(questionnaire)[4] = "L1"
names(questionnaire)[6] = "eng_aoa"
names(questionnaire)[7] = "span_aoa"
names(questionnaire)[8] = "port_aoa"
names(questionnaire)[9] = "prolific_id"

## Combine all Lextale dataframes and the questionnaire
lextale_df = left_join(questionnaire, 
                     eng_lextale, by = "gorilla_id") %>% 
  left_join(port_lextale, by = "gorilla_id") %>%
  left_join(span_lextale, by = "gorilla_id") %>% 
  select(gorilla_id, L1, eng_aoa,
         span_aoa, port_aoa, prolific_id, lextale_score_eng,
         lextale_score_port, lextale_score_span)

## Save the output  
lextale_df %>% 
  write.csv(here("data", "tidy", "lextale_df.csv"))





