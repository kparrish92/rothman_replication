## Sample size for alpha = 0.05, 80% power, equivalence bounds of
## Cohen's d = -0.4 and Cohen's d = 0.4, assuming true effect = 0
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
sesoi = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
)

powerTOSTtwo(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.2, high_eqbound_d=0.2)

powerTOSTtwo(N = 18, alpha=0.05, low_eqbound_d=-0.4, high_eqbound_d=0.4)
powerTOSTtwo(N = 17, alpha=0.05, statistical_power=0.8)



power_curve_tost_df %>% 
  ggplot(aes(x = sesoi, y = num_needed)) + geom_point() + geom_line() 

####

# The required sample size to achieve 80 % power 
# with equivalence bounds of -0.1 and 0.1 is 1713 per group, 
# or 3426 in total.
#
# The plot shows the required sample size to achieve 80% power given
# equivalence bounds that are equivalent to the sesoi. 
# In other words, these eq bounds show the percentage of time the 
# observed effect size/ es of the samples is less than the sesoi. 

# Cohen (1988) recommened effect sizes of d = .... as a guideline, but 
# stated that theory should ultimately inform what consitutes a meaningful
# effect. 
# In an analysis of effect size in L2 research, Plonsky and Oswald found that 
# a small effect size was d = +/- .4


power_t_TOST(
  n = NULL,
  eqb = .4,
  low_eqbound = NULL,
  high_eqbound = NULL,
  alpha = .05,
  power = .8,
  type = "two.sample"
)

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


