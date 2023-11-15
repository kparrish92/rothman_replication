# ------------------------------------------------------
# Date 11/9/23
# This script creates the plots used in the manuscript
# There are 5 total plots
# -------------------------------------------------------

## Load Libraries and data
source(here::here("scripts", "00_libs.R"))
source(here("scripts", "04_load_data.R"))

## Tidy collocation task for proficiency in each language per group 
prof_df = collocation_task %>% 
  group_by(prolific_id, L1) %>% 
  summarise(English = mean(lextale_score_eng), 
            Spanish = mean(lextale_score_span), 
            Portuguese = mean(lextale_score_port)) %>% 
  pivot_longer(cols = c("English", "Spanish", "Portuguese"), 
               names_to = "Language", 
               values_to = "Score")
  
## Figure 1: LexTALE score as a function of Language and Group
prof_df %>% 
  ggplot(aes(y = Score, x = Language, fill = L1)) + 
  geom_boxplot() + theme_minimal() +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  ggsave(here("docs", "figs", "prof_plot.png"))

## Tidy context-based Collocation Task for plotting
desc_all = collocation_task %>% 
  group_by(prolific_id, pre_post, L1) %>% 
  summarize(n_correct = sum(is_correct))   

mean_correct = desc_all %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct),
            sd_c = sd(n_correct))

## Figure 2: Average Number of correct answers in the Context-based Collocation Task
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

## Tidy Interpretation Task for plotting
desc_all_i = interpretation_task %>% 
  group_by(prolific_id, pre_post, L1) %>% 
  summarize(n_correct = sum(is_correct))   

mean_correct_i = desc_all_i %>% 
  group_by(L1, pre_post) %>% 
  summarize(mean_c = mean(n_correct),
            sd_c = sd(n_correct))

## Figure 3: Average Number of correct answers in the Semantic Interpretation Task
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

## Generate data for Figure 4
power_curve_tost_df = data.frame(num_needed = round(c(powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.1, high_eqbound_d=0.1),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.2, high_eqbound_d=0.2),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.3, high_eqbound_d=0.3),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.4, high_eqbound_d=0.4),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.5, high_eqbound_d=0.5),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.6, high_eqbound_d=0.6),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.7, high_eqbound_d=0.7),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.8, high_eqbound_d=0.8),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.9, high_eqbound_d=0.9),
                                                      powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-1, high_eqbound_d=1))),
                                 sesoi = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1))


## Plot Figure 4
power_curve_tost_df %>% 
  ggplot(aes(x = as.factor(sesoi), y = num_needed, label = num_needed)) +
  geom_col(fill = "seagreen", color = "black") +
  geom_label(nudge_y = 100) +
  xlab("Smallest Effect Size of Interest") + 
  ylab("Participants per group") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggsave(here("docs", "figs", "pc.png"))


## Generate data for Figure 5 
power_curve_t_df = data.frame(num_needed = round(c(pwr.2p.test(h = .1, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .2, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .3, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .4, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .5, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .6, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .7, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .8, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = .9, n = NULL, sig.level = 0.05, power = .8)[["n"]],
                                                   pwr.2p.test(h = 1, n = NULL, sig.level = 0.05, power = .8)[["n"]])),
                              sesoi = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1))


## Plot Figure 5 
power_curve_t_df %>% 
  ggplot(aes(x = as.factor(sesoi), y = num_needed, label = num_needed)) +
  geom_col(fill = "skyblue", color = "black") +
  geom_label(nudge_y = 100) +
  xlab("Smallest Effect Size of Interest") + 
  ylab("Participants per group") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  ggsave(here("docs", "figs", "pc_t.png"))

