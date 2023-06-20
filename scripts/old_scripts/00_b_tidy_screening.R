

sc_dat = read.csv(here("data", "screening", "screening.csv")) %>% 
  rename(l1 = `My.first.language.is...`) %>% 
  rename(l2 = `My.second.language.is...`) %>% 
  rename(l3 = `My.third.language.is...`) %>% 
  filter(l3 == "Portuguese") %>% 
  rename(prolific_id = `What.is.your.prolific.id.`)


eng_l1_ids = sc_dat %>% 
  filter(l1 == "English" & l2 == "Spanish") %>% 
  filter(Do.you.speak.Spanish..English.and.Portuguese..Please.answer.yes..even.if.you.re.still.learning.one.of.these.languages. == "yes")

span_l1_ids = sc_dat %>% 
  filter(l1 == "Spanish" & l2 == "English") %>% 
  filter(Do.you.speak.Spanish..English.and.Portuguese..Please.answer.yes..even.if.you.re.still.learning.one.of.these.languages. == "yes")

span_l1_ids$prolific_id
eng_l1_ids$prolific_id



port_speakers = read.csv(here("data", "screening", 
                              "port_controls.csv"))



