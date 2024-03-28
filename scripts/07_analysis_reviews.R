## Additional tests requested by reviewers 

## Load Libraries and data.
source(here::here("scripts", "00_libs.R"))
source(here("scripts", "04_load_data.R"))

## Tidy aoa for analysis 

aoa_df = collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarize(eng = mean(eng_aoa), 
            span = mean(span_aoa), 
            port = mean(port_aoa))  %>% 
  pivot_longer(cols = c(eng:port), names_to = "language",
                                                  values_to = "score")


## L1 English group AOA L2 vs L3

aoa_eng_comp = aoa_df %>% filter(L1 == "English" & language != "eng")

aoa_e_c_t = t.test(score ~ language, data = aoa_eng_comp)
es_aoa_e_c_t = cohens_d(score ~ language, data = aoa_eng_comp)



## L1 Spanish group AOA L2 vs L3

aoa_span_comp = aoa_df %>% filter(L1 == "Spanish" & language != "span")

aoa_s_c_t = t.test(score ~ language, data = aoa_span_comp)
es_aoa_s_c_t = cohens_d(score ~ language, data = aoa_span_comp)




## Tidy a dataframe for analysis 
proficiency_df = collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarize(eng = mean(lextale_score_eng_std), 
            span = mean(lextale_score_span_std), 
            port = mean(lextale_score_port_std)) %>% 
  pivot_longer(cols = c(eng:port), names_to = "language",
               values_to = "score")

## L1 English speakers are more proficient than in their L1

e_df = proficiency_df %>% filter(language == "eng")

eng_prof_t_test = t.test(score ~ L1, data = e_df)
es_eng_prof_t_test = cohens_d(score ~ L1, data = e_df)

## L1 Spanish speakers are more proficient than in their L1

s_df = proficiency_df %>% filter(language == "span")

span_prof_t_test = t.test(score ~ L1, data = s_df)
es_span_prof_t_test = cohens_d(score ~ L1, data = s_df)

## The English L1 groups in the L2 and L3 

e_in_df = proficiency_df %>% filter(L1 == "English") %>% 
  filter(language == "span" | language == "port")


e_l2_l3_t_test = t.test(score ~ language, paired = TRUE, data = e_in_df)
es_e_l2_l23 = cohens_d(score ~ language, data = e_in_df)

## The Spanish L1 groups in the L2 and L3 

s_in_df = proficiency_df %>% filter(L1 == "Spanish") %>% 
  filter(language == "eng" | language == "port")


s_l2_l3_t_test = t.test(score ~ language, paired = TRUE, data = s_in_df)
es_s_l2_l23 = cohens_d(score ~ language, data = s_in_df)


# Compare L2s between groups 


l2_comp = proficiency_df %>% 
  filter(language == "eng" & L1 == "Spanish" | language == "span" & L1 == "English")

l2_comp_t_test = t.test(score ~ L1, data = l2_comp)
es_l2_comp = cohens_d(score ~ L1, data = l2_comp)

# Compare L3s 


l3_comp = proficiency_df %>% 
  filter(language == "port")

l3_comp_t_test = t.test(score ~ L1, data = l3_comp)
es_l3_comp = cohens_d(score ~ L1, data = l3_comp)





#p_mod = brms::brm(score ~ L1*language + (1 | prolific_id), data = proficiency_df)

#conditional_effects(p_mod)