##############################################################################
# File-Name: nalysis_sif_survey.r
# Date: January 21 2023
# author: Tiago Ventura 
# Purpose: this code generates the results presented in the sif
# Data in: "data/processed_survey_data.rds"
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.2 LTS
##############################################################################


# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(summarytools)
library(lubridate)
library(scales)
library(conflicted)
library(rebus)
library(patchwork)
library(extrafont)
library(broom)
library(here)
source(here("code", "utils.R"))
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# Open data ---------------------------------------------------------------
conjoint_data =read_rds(here("data", "processed_survey_data.rds"))

# clean covariates -------------------------------------------------------

# Covariates --------------------------------------------------------------
# fear of crime
conjoint_data <- conjoint_data %>% 
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x)) %>%
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              fct_relevel(.x, "Muy Seguro", "Seguro", "Poco Seguro")) %>%
  rowwise() %>%
  mutate(fear_av=mean(c(fear_1, fear_2, fear_3), na.rm = TRUE)) %>%
  ungroup()

# victimization
list_vict=list("Not a Victim"="No", 
               "Victim"="Si")

conjoint_data <- conjoint_data %>% 
  mutate_at(vars(crime_victimization, police_victimization), ~
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x)) %>%        
  mutate_at(vars(crime_victimization, police_victimization), ~
              fct_recode(.x,!!!list_vict))

table(conjoint_data$crime_victimization)

# Preferences for Law and Order
list_lo_invert <- list("0"="5",
                       "1"="4",
                       "3"="3", 
                       "4"="1",
                       "5"="0")

conjoint_data <- conjoint_data %>%  
  mutate_at(vars(lo_1, lo_2, 
                 lo_3, lo_4, 
                 lo_5), ~ na_if(.x, "-999")) %>%
  mutate_at(vars(lo_4, lo_5), ~ fct_recode(.x, !!!list_lo_invert)) %>% 
  mutate_at(vars(lo_1, lo_2, lo_3, lo_4, lo_5), ~ as.numeric(as.character(.x))) %>%
  rowwise() %>%
  mutate(lo_av=mean(c(lo_1, lo_2, lo_3, lo_4, lo_5), na.rm = TRUE))


# Modeling ----------------------------------------------------------------

# Gender
levels_gender <- levels(conjoint_data$feat_gender)
levels_gender <- list("Male"= levels_gender[2], 
                      "Female" = levels_gender[1])

# Party

levels_party <- levels(conjoint_data$feat_political_party)
levels_party <- list("PRI"="PRI", "MORENA"="Morena", "PAN"="PAN", 
                     "Independents"="Independiente")

# Occupation
levels_occup <- levels(conjoint_data$feat_occupation)
levels_occup <- list("Leader Autodefensas" = levels_occup[2], 
                     "Chief Local Police" = levels_occup[3], 
                     "Human-Rights Activist"=levels_occup[4], 
                     "Owner Private Security Firm"=levels_occup[5], 
                     "Public Employee"=levels_occup[1])

# Proposal
levels_sec <- levels(conjoint_data$feat_security_proposal)
levels_sec <- list("Education to Youth"= levels_sec[2], 
                   "Better Police/Security Cameras"= levels_sec[3],
                   "Death Penalty"= levels_sec[4],
                   "Police Militarization"= levels_sec[5], 
                   "Victims Oriented Policies"= levels_sec[1])

features = c("political_party", "gender", "occupation", "security_proposal")
references <- c(levels(conjoint_data$feat_political_party)[1],
                levels(conjoint_data$feat_gender)[1],
                levels(conjoint_data$feat_occupation)[1], 
                levels(conjoint_data$feat_security_proposal)[1])

list_terms <- c("Political Party", paste0("    ", names(levels_party)), " ",
                "Gender", paste0("    ", names(levels_gender)),"  ", 
                "Occupation", paste0("    ", names(levels_occup)), "   ",
                "Security Proposal", paste0("    ", names(levels_sec)), "    ")

# function to plot the conjoint results
tidy_conjoint_model <- function(model, subsample, features=features,
                                references=references, list_terms){
  
  
  
  # to make it more general, it is just to change the  4 for the number of features
  model %>% 
    tidy(.) %>% 
    mutate(lb=estimate - 1.96*std.error, 
           up= estimate + 1.96*std.error, 
           lb90=estimate - 1.64*std.error, 
           up90= estimate + 1.64*std.error) %>% 
    filter(!(term %in% c("(Intercept)"))) %>% 
    mutate(term=str_remove_all(term, paste0("feat_", features, collapse = "|")),
           term=str_replace_all(term, "_", " "),
           term=as.factor(term), 
           term=str_c("    ", term)) %>%
    bind_rows(data_frame(term=c(str_to_title(str_replace_all(features, "_", " ")), 
                                " ", "  ", "   ", "    ",
                                paste0("    ", references)), 
                         estimate=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         std.error=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         statistic=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         p.value=c(rep(NA, 4), rep(NA, 4), rep(0,4)), 
                         lb=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         up=c(rep(NA, 4), rep(NA, 4), rep(0,4)), 
                         lb90=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         up90=c(rep(NA, 4), rep(NA, 4), rep(0,4)))) %>% 
    mutate(subsample=subsample, 
           term=fct_relevel(term, list_terms), 
           term=fct_rev(term))
}

# Create the main moderators

conjoint_data <- conjoint_data %>%
  mutate(cv=crime_victimization, 
         fs=feat_security_proposal, 
         fo=feat_occupation) 


# Crime Vitimization: direct survey measure ------------------------------------------------------
library(estimatr)

# Model
res_vict <- conjoint_data %>% 
  group_by(crime_victimization) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, crime_victimization, 
                    ~ lm_robust(outcome_num~ feat_political_party + feat_gender + 
                                  feat_security_proposal + feat_occupation,
                                clusters=responseid,
                                data=.x) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model) %>% select(-data)

# Remove repeated
res_vict_color <- res_vict %>% ungroup() %>% 
  mutate(id_remove=ifelse(crime_victimization=="Victim"& (is.na(estimate) | estimate==0.0000), 1, 0)) %>%
  filter(id_remove==0) 

ggplot(res_vict_color, aes(y=estimate, x=term, 
                           ymin=up, ymax=lb, shape=crime_victimization)) +
  geom_pointrange(size=1, fill="black", color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_shape_manual(values=c(22,23), name="Crime Victimization") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))

ggsave(filename=here("output", "fig15.png") ,
               width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Marginal Effects

mef_vict <- conjoint_data %>% 
  mutate(cv=crime_victimization, 
         cv=fct_relevel(cv, "Not a Victim")) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              cv*fs + cv*fo, 
            data=., 
            clusters = responseid)

# Marginal Effects

# Get names: Constitutive Terms
list_ct <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                "fsDeath Penalty", "fsPolice Militarization")

list_interactive <- as.list(paste0("cvVictim:", list_ct))

res_vict <- map2_df(list_ct, list_interactive, ~ 
                      interaction_plot_binary(
                        mef_vict, 
                        effect = .x, 
                        moderator = "cvVictim", 
                        interaction = .y) %>%
                      mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")), 
         moderator=ifelse(moderator==1, "Victim", "Not Victim"))


ggplot(res_vict, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, 
           x=treatment, shape=moderator)) +
  geom_pointrange(size=1.5, fill="white",
                  color="gray70", 
                  position=position_dodge(width = .6), alpha=.8) +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  scale_shape_manual(values=c(22,23), name="Crime Victimization") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred")  +
  theme(axis.text.x =  element_text(size=12))


ggsave(filename=here("output", "fig6a.png") ,
         width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Police Violence: direct survey measures -------------------------------------------------


# Model

res_pol <- conjoint_data %>% 
  group_by(police_victimization) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, police_victimization, 
                    ~ lm_robust(outcome_num~ feat_political_party + feat_gender + 
                                  feat_security_proposal + feat_occupation,
                                clusters=responseid,
                                data=.x) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model) %>% select(-data)

# Remove repeated
res_pol_color <- res_pol %>% ungroup() %>% 
  mutate(id_remove=ifelse(police_victimization=="Victim"& (is.na(estimate) | estimate==0.0000), 1, 0),
         police_victimization=fct_rev(police_victimization)) %>%
  filter(id_remove==0) 


ggplot(res_pol_color, aes(y=estimate, x=term, 
                          ymin=up, ymax=lb, shape=police_victimization)) +
  geom_pointrange(size=1, fill="black", color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_shape_manual(values=c(22,23), 
                     name="Police Victimization", 
                     guide = guide_legend(reverse = TRUE)) +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))

#ggsave(filename=here("output", "fig16.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Marginal Effects

mef_vict <- conjoint_data %>% 
  mutate(cv=police_victimization, 
         cv=fct_relevel(cv, "Not a Victim")) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              cv*fs + cv*fo, 
            data=., 
            clusters = responseid)


# Marginal Effects

# Trust invest
mef_vict %>% tidy() %>% pull(term)

# Get names: Constitutive Terms

list_ct <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                "fsDeath Penalty", "fsPolice Militarization")

list_interactive <- as.list(paste0("cvVictim:", list_ct))

res_vict <- map2_df(list_ct, list_interactive, ~ 
                      interaction_plot_binary(
                        mef_vict, 
                        effect = .x, 
                        moderator = "cvVictim", 
                        interaction = .y) %>%
                      mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")), 
         moderator=ifelse(moderator==1, "Victim", "Not Victim"))


ggplot(res_vict, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, 
           x=treatment, shape=moderator)) +
  geom_pointrange(size=1.5, fill="white",
                  color="gray70", 
                  position=position_dodge(width = .6), alpha=.8) +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  scale_shape_manual(values=c(22,23), name="Victim Police Violence") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred")  +
  theme(axis.text.x =  element_text(size=12))


ggsave(filename=here("output", "fig6b.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Results Network Measures of Crime ---------------------------------------------------------
# clean network raw data
conjoint_data <- conjoint_data %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999"))))

# get results from network models
load(here("data", "network_results.Rdata"))

# convert to wide
crime_wide <- crime %>% 
  pivot_wider(names_from=group, 
              values_from = c(yhat, res, values))

# join
conjoint_data <- left_join(conjoint_data, crime_wide)


# modeling heterogenous effects
mef_fs <- conjoint_data %>% 
  mutate(nvc=res_network_victim_crime) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              nvc*fs + fo, 
            data=., clusters = responseid)

# save model with a different name
mod_cv <- mef_fs


mef_fo <- conjoint_data %>% 
  mutate(nvc=res_network_victim_crime) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              fs + nvc*fo, 
            data=., clusters=responseid)


# Marginal Effects
mef_fs %>% tidy() %>% pull(term)
mef_fo %>% tidy() %>% pull(term)

# Get names: Constitutive Terms
list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("nvc:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("nvc:", list_ct_fo))



# Mef FS
mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      mef_fs, 
                      effect = .x, 
                      moderator = "nvc", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

# fo

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      mef_fo, 
                      effect = .x, 
                      moderator = "nvc", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))



# Results with quantiles ----------------------------------------------------------


c_t <- conjoint_data %>%
  ungroup() %>%
  mutate(quant=ntile(res_network_victim_crime, 5),
         quant_1_5=case_when(quant=="1" ~ "2sd_minus",
                             quant=="5" ~ "2sd_plus"))


mod_fs =c_t %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender +
              quant_1_5*fs,
            data=.)


mod_fs =c_t %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender +
              quant_1_5*fs + fo,
            data=.)

summary(mod_fs)
car::linearHypothesis(mod_fs, c("quant_1_52sd_plus:fsDeath Penalty +quant_1_52sd_plus =  fsDeath Penalty  "))

mod_fo =c_t %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender +
              fs + quant_1_5*fo,
            data=.)

summary(mod_fo)
car::linearHypothesis(mod_fo, c("quant_1_52sd_plus:foChief Local Police + quant_1_52sd_plus = foChief Local Police"))
car::linearHypothesis(mod_fo, c("quant_1_52sd_plus:foOwner Private Security Firm  + quant_1_52sd_plus =  foOwner Private Security Firm"))

# Get names: Constitutive Terms
list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("quant_1_52sd_plus:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("quant_1_52sd_plus:", list_ct_fo))


# Mef FS
mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_binary(
                      mod_fs, 
                      effect = .x, 
                      moderator = "quant_1_52sd_plus:", 
                      interaction = .y) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")), 
         moderator=case_when(moderator==0 ~ "First Quantile", 
                             moderator==1 ~ "Last Quantile"), 
         moderator=fct_relevel(moderator, "Last Quantile"))

# fo

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_binary(
                      mod_fo, 
                      effect = .x, 
                      moderator = "quant_1_52sd_plus:", 
                      interaction = .y) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")), 
         moderator=fct_rev(case_when(moderator==0 ~ "First Quantile", 
                                     moderator==1 ~ "Last Quantile")))


# Plot Marginal Effects Interactive

ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, fill=moderator, 
           x=treatment)) +
  geom_pointrange(shape=21, size=1.5, color="gray70", 
                  position=position_dodge(width = .6), alpha=.8) +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="", 
       y="Marginal Effect", 
       x="", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  scale_fill_manual(  values=c("steelblue", "tomato2"), 
                      name="Contextual Exposure to Crime", 
                      guide=guide_legend(reverse=TRUE)) +
  geom_segment(aes(x=2.8, xend=3.2, y=0.19, yend=0.19), size=.5, 
               alpha=.4) +
  geom_segment(aes(x=3.2, xend=3.2, y=0.19, yend=0.185), size=.5, 
               alpha=.4) +
  geom_segment(aes(x=2.8, xend=2.8, y=0.19, yend=0.185), size=.5, 
               alpha=.4) +
  geom_text(aes(x=3, y=.15, label="p-value= 0.078", family=""),
            size=4, family=my_font, color = "black") +
  ylim(-.2, .3) +
  coord_flip()


#ggsave(filename=here("output", "fig3.png") ,
#       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


library(extrafont)
my_font <- "Palatino Linotype"

ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, fill=moderator, 
           x=treatment)) +
  geom_pointrange(shape=21, size=1.5, color="gray70", 
                  position=position_dodge(width = .6), alpha=.8) +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="", 
       y="Marginal Effect", 
       x="", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  scale_fill_manual(  values=c("steelblue", "tomato2"), 
                      name="Contextual Exposure to Crime", 
                      guide=guide_legend(reverse=TRUE)) +
  geom_segment(aes(x=1.8, xend=2.2, y=0.13, yend=0.13), size=.1, 
               alpha=.4) +
  geom_segment(aes(x=2.2, xend=2.2, y=0.13, yend=0.11), size=.4, 
               alpha=.4) +
  geom_segment(aes(x=1.8, xend=1.8, y=0.13, yend=0.11), size=.4, 
               alpha=.4) +
  geom_text(aes(x=2, y=.16, label="p-value= 0.06", family=""),
            size=4, family=my_font, color = "black") +
  ylim(-.2, .3) +
  coord_flip()


#ggsave(filename=here("output","fig4.png") ,
#       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Results using Network Measures for Police Violence ----------------------------------------------------

#model

mef_fs <- conjoint_data %>% 
  mutate(nvc=res_network_police_violence) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              nvc*fs + fo, 
            data=., 
            clusters=responseid)

# change model name
mod_pv <- mef_fs

mef_fo <- conjoint_data %>% 
  mutate(nvc=res_network_police_violence) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              fs + nvc*fo, 
            data=., 
            clusters=responseid)



# Marginal Effects

# Get names: Constitutive Terms
list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("nvc:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("nvc:", list_ct_fo))

# Mef FS
mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      mef_fs, 
                      effect = .x, 
                      moderator = "nvc", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

# fo
mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      mef_fo, 
                      effect = .x, 
                      moderator = "nvc", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))


# Plot Marginal Effects Interactive


ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="", 
       y="Marginal Effect", 
       x="Overdispersion of Police Violence in the Respondents Network", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 

# ggsave(filename=here("output", "fig2a.png") ,
#         width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Overdispersion of Police Violence in the Respondents Network", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 

# ggsave(filename=here("output", "fig2b.png") ,
#          width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Models with Het for Fear of Crime -----------------------------------------------------------
mef_fs <- conjoint_data %>% 
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              fear_av*fs + fo, 
            data=., 
            clusters=responseid)

mef_fo <- conjoint_data %>% 
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
              fs + fear_av*fo, 
            data=., 
            clusters=responseid)


# Get names: Constitutive Terms
list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("fear_av:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("fear_av:", list_ct_fo))


# Mef FS


mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      mef_fs, 
                      effect = .x, 
                      moderator = "fear_av", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

# fo

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      mef_fo, 
                      effect = .x, 
                      moderator = "fear_av", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))


# Plot Marginal Effects Interactive
ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Fear of Crime", 
       caption="Aggregated Scale from Three Questions About Fear of Crime. Scale goes from Not Safe to Very Safe. 
              Baseline Condition: Candidate Proposing Victims Oriented Policies") +
  facet_wrap(~treatment, ncol=2) 

ggsave(filename=here("output", "fig10a.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Fear of Crime", 
       caption="Aggregated Scale from Three Questions About Fear of Crime. Scale goes from Not Safe to Very Safe.
       Baseline Condition: Public Employee.") +
  facet_wrap(~treatment, ncol=2) 

ggsave(filename=here("output", "fig10b.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Het for Law and Order Preferences -----------------------------------------------------------

# notice I am inverting the direction of the questions here. It is easier to get the mean

# Just keep numerics
conjoint_data %>% select(responseid, ipaddress, lo_1, lo_2, lo_3, lo_4, lo_5, 
                         lo_av) %>% 
  filter(ipaddress=="189.190.190.15")



models <- tibble::tribble(
  ~model_name,    ~ formula, ~data,
  "interactive-fs", outcome_num ~ feat_political_party + feat_gender +
    lo_av*fs + fo, conjoint_data, 
  "interactive-fo",  outcome_num ~ feat_political_party + feat_gender +
    fs + lo_av*fo, conjoint_data, 
  "interactive-party",  outcome_num ~ lo_av*feat_political_party + feat_gender +
    fs + fo, conjoint_data
  
)


models <- models %>% 
  rowwise(model_name) %>% 
  mutate(model = list(lm(formula, data = data))) 

# Get names: Constitutive Terms
models$model[[3]] %>% tidy() %>% pull(term)

list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("lo_av:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("lo_av:", list_ct_fo))

list_ct_pt <- list("feat_political_partyPRI", "feat_political_partyMORENA",
                   "feat_political_partyPAN")

list_interactive_pt <- as.list(paste0("lo_av:", list_ct_pt))

# Mef FS

mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[1]], 
                      effect = .x, 
                      moderator = "lo_av", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

# fo

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[2]], 
                      effect = .x, 
                      moderator = "lo_av", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))

# party

mef_pt <- map2_df(list_ct_pt, list_interactive_pt, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[3]], 
                      effect = .x, 
                      moderator = "lo_av", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "feat_political_party")))



# Plot Marginal Effects Interactive
ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Support for Law and Order", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 


ggsave(filename=here("output", "fig11a.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Support for Law and Order", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 


ggsave(filename=here("output", "fig11b.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Het effect by Trust In the Police -----------------------------------------------------

conjoint_data <- conjoint_data %>%
  mutate_at(vars(trust_amlo, trust_police), ~as.numeric(.x)) %>%
  mutate_at(vars(trust_amlo, trust_police), ~ ifelse(.x<0, NA,
                                                     .x))
# Models: This is really, really nice
models <- tibble::tribble(
  ~model_name,    ~ formula, ~data,
  "interactive-fs", outcome_num ~ feat_political_party + feat_gender +
    trust_police*fs + fo, conjoint_data, 
  "interactive-fo",  outcome_num ~ feat_political_party + feat_gender +
    fs + trust_police*fo, conjoint_data, 
  "interactive-party",  outcome_num ~ trust_police*feat_political_party + feat_gender +
    fs + fo, conjoint_data)

models <- models %>% 
  rowwise(model_name) %>% 
  mutate(model = list(lm(formula, data = data))) 

summary(models$model[[1]])
summary(models$model[[2]])
summary(models$model[[3]])

# Get names: Constitutive Terms

list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("trust_police:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("trust_police:", list_ct_fo))

list_ct_pt <- list("feat_political_partyPRI", "feat_political_partyMORENA",
                   "feat_political_partyPAN")

list_interactive_pt <- as.list(paste0("trust_police:", list_ct_pt))

# Mef FS

mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[1]], 
                      effect = .x, 
                      moderator = "trust_police", 
                      interaction = .y, 
                      num_points = 10) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

# fo

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[2]], 
                      effect = .x, 
                      moderator = "trust_police", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))

# party

mef_pt <- map2_df(list_ct_pt, list_interactive_pt, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[3]], 
                      effect = .x, 
                      moderator = "trust_police", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "feat_political_party")))



# Plot Marginal Effects Interactive
ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Trust in the Police", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 

ggsave(filename=here("output", "fig9a.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Trust in the Police", 
       caption="Baseline Condition: Public Employee ") +
  facet_wrap(~treatment, ncol=2) 

ggsave(filename=here("output", "fig9b.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Het by Income ------------------------------------------------------------------

models <- tibble::tribble(
  ~model_name,    ~ formula, ~data,
  "interactive-fs", outcome_num ~ feat_political_party + feat_gender +
    income*fs + fo, conjoint_data, 
  "interactive-fo",  outcome_num ~ feat_political_party + feat_gender +
    fs + income*fo, conjoint_data)


models <- models %>% 
  rowwise(model_name) %>% 
  mutate(model = list(lm(formula, data = data))) 



# Get names: Constitutive Terms
models$model[[1]] %>% tidy() %>% pull(term)

list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                   "fsDeath Penalty", "fsPolice Militarization")

list_interactive_fs <- as.list(paste0("income:", list_ct_fs))

list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
                   "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive_fo <- as.list(paste0("income:", list_ct_fo))


# Mef FS

mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[1]], 
                      effect = .x, 
                      moderator = "income", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fs")))

# fo

mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
                    interaction_plot_continuous_ggplot(
                      models$model[[2]], 
                      effect = .x, 
                      moderator = "income", 
                      interaction = .y, 
                      num_points = 11) %>%
                    mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")))


# Plot Marginal Effects Interactive
ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Support for Law and Order", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 

# ggsave(filename=here("output", "fig8a.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Income", 
       caption="Baseline Condition: Public Employee ") +
  facet_wrap(~treatment, ncol=2) 


ggsave(filename=here("output", "fig8b.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Heterogenous effects by gender ------------------------------------------------------

# Model
res_gender <- conjoint_data %>% 
  group_by(gender) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, gender, 
                    ~ lm_robust(outcome_num~ feat_political_party + feat_gender + 
                                  feat_security_proposal + feat_occupation,
                                clusters=responseid,
                                data=.x) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model) %>% select(-data)

# Remove repeated
res_gender_color <- res_gender %>% ungroup() %>% 
  mutate(id_remove=ifelse(gender=="Male"& (is.na(estimate) | estimate==0.0000), 1, 0),
         gender=fct_rev(gender)) %>%
  filter(id_remove==0) 

ggplot(res_gender_color, aes(y=estimate, x=term, 
                             ymin=up, ymax=lb, shape=gender)) +
  geom_pointrange(size=1, fill="black", color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_shape_manual(values=c(22,23), name="Respondents' Gender") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))

ggsave(filename=here("output", "fig7.png") ,
      width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Analysis of Partisanship: appendix D ------------------------------------------------------------

conjoint_data <- conjoint_data %>% 
  mutate(negative_partisanship_recode=ifelse(str_detect(negative_partisanship, "MORENA") &
                                               str_detect(positive_partisanship, "Ninguno|No responde|No sabe"),
                                             "Anti-Morena", 
                                             ifelse(str_detect(negative_partisanship, "PRI") & 
                                                      str_detect(positive_partisanship, "Ninguno|No responde|No sabe"),
                                                    "Anti-Pri",
                                                    ifelse(str_detect(negative_partisanship, "PAN") &
                                                             str_detect(positive_partisanship, "Ninguno|No responde|No sabe"), 
                                                           "Anti-Pan", NA))))

conjoint_data <- conjoint_data %>%
  mutate(positive_partisanship_recode=ifelse(
    str_detect(positive_partisanship, "(MORENA)"), 
    "MORENA Voters", 
    ifelse(str_detect(positive_partisanship, "(PAN)"), 
           "PAN Voters",
           ifelse(str_detect(positive_partisanship, "(PRI)"), 
                  "PRI Voters", "Independents"))))



# By negative partisanship
res_negative <- conjoint_data %>% 
  nest_by(negative_partisanship_recode) %>% 
  drop_na() %>%
  mutate(model=map2(data, negative_partisanship_recode, 
                    ~ lm_robust(outcome_num~ feat_political_party + feat_gender + 
                                  feat_security_proposal + feat_occupation, 
                                data=.x, 
                                clusters = responseid) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model)

# By positive partisanship

res_positive <- conjoint_data %>% 
  nest_by(positive_partisanship_recode) %>% 
  drop_na() %>%
  mutate(model=map2(data, positive_partisanship_recode, 
                    ~ lm_robust(outcome_num~ feat_political_party + feat_gender + 
                                  feat_security_proposal + feat_occupation, 
                                data=.x, 
                                clusters=responseid) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model)

# Change graph a bit

reframe_graph <- function(data, levels_gender=levels_gender, 
                          levels_party=levels_party, 
                          levels_occup=levels_occup, 
                          levels_sec=levels_sec){
  
  data %>%
    mutate(term_c=str_trim(term), 
           features=ifelse(term_c%in% names(levels_gender), 
                           "Gender", 
                           ifelse(term_c%in% names(levels_party), 
                                  "Party", 
                                  ifelse(term_c %in% names(levels_occup), 
                                         "Occupation",
                                         ifelse(term_c%in% names(levels_sec), 
                                                "Security Proposal", NA)))), 
           features=fct_relevel(features, "Party", "Gender", 
                                "Occupation", "Security Proposal")) 
}


res_negative_r <- reframe_graph(data=res_negative, levels_gender, 
                                levels_party, levels_occup, levels_sec) %>%
  drop_na() %>%
  mutate(subsample=fct_relevel(subsample, "Anti-Morena", "Anti-Pan"))


res_positive_r <- reframe_graph(data=res_positive, levels_gender, 
                                levels_party, levels_occup, levels_sec)  %>%
  drop_na() %>%
  mutate(subsample=fct_relevel(subsample, "MORENA Voters",
                               "PAN Voters", "PRI Voters"))




# filter just the party

res_negative_r <- res_negative_r %>%
  filter(features=="Party")

res_positive_r <- res_positive_r %>%
  filter(features=="Party", positive_partisanship_recode!="Independents")

# POSITIVE

ggplot(res_positive_r, aes(y=estimate, x=term_c, 
                           ymin=up, ymax=lb, fill=subsample)) +
  geom_pointrange(size=1.5, shape=21, color="grey5",
                  position=position_dodge(width = .8), alpha=.8) +
  labs(x="", y="AMCE (Subgrup)", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_fill_manual(values=c(pal[1],pal[9], "Green", pal[5]), 
                    name="")  +
  coord_flip() + 
  facet_grid(features~subsample, scales = "free") +
  theme(axis.text.y=element_text(size=16)) 


#ggsave(filename=here("output", "fig5a.png") ,
#       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Negative Partisanship

ggplot(res_negative_r, aes(y=estimate, x=term_c, 
                           ymin=up, ymax=lb, fill=subsample)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="AMCE (Subgroup)", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_fill_manual(values=c(pal[1],pal[9], "Green", pal[5]), 
                    name="Negative Partisanship")  + coord_flip() + 
  facet_grid(features~subsample, scales="free") +
  theme(axis.text.y=element_text(size=16,hjust=1))

# ggsave(filename=here("output", "fig5b.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

