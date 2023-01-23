# ------------------------------------------------------------------------------- #
# Article: Voting for Law and Order in Mexico
# Authors: Cantu, Ley, Ventura
# Update: June 15
# ------------------------------------------------------------------------------- #
library(here)

# Packages ----------------------------------------------------------------
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


# Paths -------------------------------------------------------------------

#path_tiago <- c("C:/Users/venturat/Dropbox/artigos/Ventura Violence/voting_for_violence_mexico_p3")
#setwd(path_tiago)

#Functions ---------------------------------------------------------------


# short view
short_view <- function(data){
  data %>% slice(1:50) %>% View()
}


# Download the data -------------------------------------------------------

load("./data/mexico_cleaned2_data.Rdata")

dim(d)

# Check Covariates --------------------------------------------------------

levels(d$education)
class(d$income)
levels(d$work)
levels(d$age)

# Cleaning the Conjoints --------------------------------------------------

## Putting together the two taks. 

outcomes <- d %>% 
  select(responseid,conjoint1_outcome, conjoint2_outcome)

outcomes <- outcomes  %>% 
  pivot_longer(cols = -responseid, 
               names_to = "var", 
               values_to = "outcome") %>% 
  mutate(task= ifelse(var=="conjoint1_outcome", "choice_1", "choice_2"), 
         candidate= ifelse(outcome=="Candidato A", "cd_1", "cd_2"), 
         missing_id= ifelse(is.na(outcome)|outcome=="-999", 1, 0), 
         candidate=ifelse(missing_id==1, "cd_1", candidate)) 

# it doesnt matter, I will exclude the missing


# The features ------------------------------------------------------------

### Clean the features


### Function to extract the features

features <- d %>% select(responseid, matches("^f_"))

var_feature_1 <- c("f_1_1", "f_1_1_1", "f_1_2_1", "f_2_1_1", "f_2_2_1")
var_feature_2 <- c("f_1_2", "f_1_1_2", "f_1_2_2", "f_2_1_2", "f_2_2_2")
var_feature_3 <- c("f_1_3", "f_1_1_3", "f_1_2_3", "f_2_1_3", "f_2_2_3")
var_feature_4 <- c("f_1_4", "f_1_1_4", "f_1_2_4", "f_2_1_4", "f_2_2_4")

# Function
cj_extract_features <- function(data, variables){
  feature <- data %>% select(responseid,variables)
  feature %>% pivot_longer(
    cols = variables[-1],
    names_to = "var",
    values_to = "value") %>% 
    mutate(task=ifelse(str_detect(var, "f_1"),"choice_1", "choice_2"), # extract the task
           candidate= ifelse(str_detect(var, DGT %R% "_1_" %R% DGT %R% END),"cd_1", "cd_2"))   %>% 
    rename("features"=variables[1]) 
}


## Extract
list_features <- list(var_feature_1, var_feature_2, var_feature_3, var_feature_4)

features_long <- map_dfr(list_features, ~ cj_extract_features(d, .x)) %>% 
  drop_na(features)

features_wider <- features_long %>% select(-var) %>%
  pivot_wider(
    names_from = features,  
    values_from= value) 


### Put together with the outcomes

# Invert the merge here. Since I just recorded in the outcome the candidate choosen, 
# I have to merge by the right, which means preserving the missings from the features data
# the missings will be the cases no choosen

conjoint_data_to_remove <- left_join(features_wider,outcomes, by=c("responseid", "task"))

id_to_remove <- conjoint_data_to_remove %>% 
  filter(missing_id==1)  %>% 
  select(responseid, task, missing_id) 

# Convert the missings to zero. But first, eliminate the real missings

conjoint_data <- left_join(features_wider,outcomes) %>% 
  anti_join(id_to_remove, by=c("responseid", "task")) %>%
  mutate(outcome_num=ifelse(is.na(outcome), 0, 1)) %>%
  select(-var, -missing_id)



# Check
table(conjoint_data$outcome_num)

# perfect = equal answers

colnames(conjoint_data) <- c("responseid", "task", "candidate", 
                             "feat_political_party", 
                             "feat_security_proposal", 
                             "feat_occupation", 
                             "feat_gender",
                             "outcome", "outcome_num")


# Get the baselines

# Check Security Policy

library(janitor)
#conjoint_data %>% tabyl(feat_security_proposal) %>% View()
#conjoint_data %>% tabyl(feat_occupation) %>% View()







# Combine -----------------------------------------------------------------
conjoint_data %>% select(responseid, ipaddress, contains("feat"), outcome_num) %>% View()
conjoint_data <- left_join(d, conjoint_data)

conjoint_data %>% tabyl(feat_occupation, outcome_num)


conjoint_data <- conjoint_data %>% 
   mutate_at(vars(contains("feat_")), as_factor) %>%
   mutate(feat_political_party=fct_relevel(feat_political_party, "Independiente"), 
          feat_gender=fct_relevel(feat_gender, "Mujer"), 
          feat_security_proposal= fct_relevel(feat_security_proposal, 
                                              "Creación de un centro de asesoría y atención a víctimas"),
          feat_occupation= fct_relevel(feat_occupation, "Empleado público"))

model <- lm(outcome_num~ feat_political_party + feat_gender + 
              feat_security_proposal + feat_occupation, data=conjoint_data) 

# Rename levels -----------------------------------------------------------

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

conjoint_data <- conjoint_data %>% 
  mutate(feat_gender=fct_recode(feat_gender, !!!levels_gender), 
         feat_political_party=fct_recode(feat_political_party, !!!levels_party), 
         feat_occupation=fct_recode(feat_occupation, !!!levels_occup), 
         feat_security_proposal=fct_recode(feat_security_proposal,
                                           !!!levels_sec)) 
levels(conjoint_data$feat_security_proposal)

model2 <- lm(outcome_num~ feat_political_party + feat_gender + 
              feat_security_proposal + feat_occupation, data=conjoint_data) 


# Covariates --------------------------------------------------------------

# fear of crime
conjoint_data <- conjoint_data %>% 
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x)) %>%
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              fct_relevel(.x, "Muy Seguro", "Seguro", "Poco Seguro")) %>%
  rowwise() %>%
  mutate(fear_av=mean(c(fear_1, fear_2, fear_3), na.rm = TRUE)) 

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

# lo
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
features = c("political_party", "gender", "occupation", "security_proposal")
references <- c(levels(conjoint_data$feat_political_party)[1],
                levels(conjoint_data$feat_gender)[1],
                levels(conjoint_data$feat_occupation)[1], 
                levels(conjoint_data$feat_security_proposal)[1])

list_terms <- c("Political Party", paste0("    ", names(levels_party)), " ",
                "Gender", paste0("    ", names(levels_gender)),"  ", 
                "Occupation", paste0("    ", names(levels_occup)), "   ",
                "Security Proposal", paste0("    ", names(levels_sec)), "    ")

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

# reduce conjoint dta

conjoint_data <- conjoint_data %>%
  mutate(cv=crime_victimization, 
         fs=feat_security_proposal, 
         fo=feat_occupation) 


# ATE

library(estimatr)

model <- lm_robust(outcome_num~ feat_political_party + feat_gender + 
              feat_security_proposal + feat_occupation, data=conjoint_data, 
              clusters = responseid,
              se_type = "stata") 

summary(model)

res <- tidy_conjoint_model(model=model, subsample="all", features = features,
                           references=references, list_terms = list_terms) %>%
        mutate(title="AMCE")

ggplot(res, aes(y=estimate, x=term, 
                ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "") +
  geom_hline(yintercept = 0, color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=14,hjust=0, face="bold"), 
        strip.text = element_text(size=16),
        panel.background = element_rect(fill = "gray95", color = NA), 
        strip.background = element_rect(fill="white"), 
        panel.grid.major = element_line(color = "white")) +
  facet_grid(~title) +
  geom_vline(xintercept = 14, linetype="dashed", color="black",alpha = .5) + #project
  geom_vline(xintercept = 18, linetype="dashed",  color="black",alpha = .5) +
  geom_vline(xintercept = 24, linetype="dashed", color="black", alpha = .5) +
  geom_vline(xintercept = 7, linetype="dashed", color="black",alpha = .5) #gov  


ggsave(filename=here("output", "no_na", "AMCE.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Crime Vitimization: Survey ------------------------------------------------------

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
  mutate(id_remove=ifelse(crime_victimization=="Victim"& (is.na(estimate) | estimate==0.0000), 1, 0),
         crime_victimization=fct_rev(crime_victimization)) %>%
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

ggsave(filename=here("output", "no_na","AMCE_crime_vict.png") ,
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

# Trust invest
mef_vict %>% tidy() %>% pull(term)

# Get names: Constitutive Terms
coef(mef_vict)

list_ct <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
                "fsDeath Penalty", "fsPolice Militarization")

list_interactive <- as.list(paste0("cvVictim:", list_ct))

interaction_plot_binary(mef_vict, effect = list_ct[[4]], 
                        moderator = "cvVictim", 
                        interaction = list_interactive[[4]])

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


 ggsave(filename=here("output", "no_na", "AICE_vict_proposal.png") ,
         width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


list_ct <- list("foLeader Autodefensas", "foChief Local Police",
                "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive <- as.list(paste0("cvVictim:", list_ct))

res_vict <- map2_df(list_ct, list_interactive, ~ 
                      interaction_plot_binary(
                        mef_vict, 
                        effect = .x, 
                        moderator = "cvVictim", 
                        interaction = .y) %>%
                      mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")), 
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

ggsave(filename=here("output", "no_na", "AICE_vict_occupation.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Police Violence: Survey -------------------------------------------------
table(conjoint_data$police_victimization)
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
         crime_victimization=fct_rev(police_victimization)) %>%
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

ggsave(filename=here("output", "no_na", "AMCE_police_vict.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

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


ggsave(filename=here("output", "no_na","AICE_vict_pol_proposal.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


list_ct <- list("foLeader Autodefensas", "foChief Local Police",
                "foHuman-Rights Activist", "foOwner Private Security Firm")

list_interactive <- as.list(paste0("cvVictim:", list_ct))

res_vict <- map2_df(list_ct, list_interactive, ~ 
                      interaction_plot_binary(
                        mef_vict, 
                        effect = .x, 
                        moderator = "cvVictim", 
                        interaction = .y) %>%
                      mutate(treatment=.x)) %>%
  mutate(treatment=fct_inorder(str_remove(treatment, "fo")), 
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

ggsave(filename=here("output", "no_na", "AICE_vict_occupation.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Poisson network : crime ---------------------------------------------------------

conjoint_data <- conjoint_data %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999"))))

load(here("data", "network_results_all_no_na.Rdata"))

crime_wide <- crime %>% 
  pivot_wider(names_from=group, 
              values_from = c(yhat, res, values))

conjoint_data <- left_join(conjoint_data, crime_wide)

# save complete data
#write_csv(conjoint_data, here("data", "data_conjoint_no_na_vict_network.csv"))
#conjoint_data <- read_csv(here("data", "data_conjoint.csv"))
#model

mef_fs <- conjoint_data %>% 
  mutate(nvc=res_network_victim_crime) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
       nvc*fs + fo, 
     data=., clusters = responseid)

summary(mef_fs)

mod_cv <- mef_fs


mef_fo <- conjoint_data %>% 
  mutate(nvc=res_network_victim_crime) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
       nvc*fs + nvc*fo, 
     data=., clusters=responseid)


# Stargazer
library(stargazer)
library(modelsummary)
names_mod=names(coef(mef_fs))
new_names = c("(Intercept)" = "Intercept", 
              "feat_political_partyPRI" = "PRI",             
              "feat_political_partyMORENA" = "MORENA",
              "feat_political_partyPAN"   ="PAN",          
              "feat_genderMale"  = "Male",
              "nvc" = "Network Residuals (NR)",                                
              "fsEducation to Youth" =   "Education to Youth",
              "fsBetter Police/Security Cameras"    = "Better Police/Security Cameras",
              "fsDeath Penalty"   = "Death Penalty",           
              "fsPolice Militarization" = "Police Militarization",             
              "foLeader Autodefensas"  = "Leader Autodefensas",       
              "foChief Local Police"   =    "Chief Local Police",          
              "foHuman-Rights Activist" = "Human-Rights Activist",        
              "foOwner Private  Security Firm" = "Owner Private  Security Firm",      
              "nvc:fsEducation to Youth"  = "NR x Education to Youth",        
              "nvc:fsBetter Police/Security Cameras" = "NR x Better Police/Security Cameras",
              "nvc:fsDeath Penalty" = "NR x Death Penalty",               
              "nvc:fsPolice Militarization" = "NR x Police Militarization")


 modelsummary(list("Crime Victimization"=mef_fs), 
              fmt = 3,
              output = here("output", "no_na", "model.tex"),
              stars = TRUE, 
              coef_map = new_names)


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


# Plot Marginal Effects Interactive


ggplot(mef_fs, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="", 
       y="Marginal Effect", 
       x="Overdispersion of Crime in the Respondents Network", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 


ggsave(filename=here("output", "no_na", "conjoint_crime_victim.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(mef_fo, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Overdispersion of Crime in the Respondents Network", 
       caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
  facet_wrap(~treatment, ncol=2) 

ggsave(filename=here("output", "no_na","conjoint_crime_victim_occup.png") ,
        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# With Deviations from the Median ----------------------------------------------------------


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
car::linearHypothesis(mod_fo, c("quant_1_52sd_plus:foHuman-Rights Activist + quant_1_52sd_plus = foHuman-Rights Activist"))
car::linearHypothesis(mod_fo, c("quant_1_52sd_plus:foOwner Private Security Firm  + quant_1_52sd_plus =  foOwner Private Security Firm"))

# Graph

mod %>% tidy() %>% pull(term)

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
  geom_text(aes(x=3, y=.15, label="p-value= 0.080", family=""),
            size=4, family=my_font, color = "black") +
  ylim(-.2, .3) +
  coord_flip()


ggsave(filename=here("output", "no_na", "conjoint_crime_victim_quantile.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

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
  geom_segment(aes(x=3.8, xend=4.2, y=0.13, yend=0.13), size=.1, 
               alpha=.4) +
  geom_segment(aes(x=4.2, xend=4.2, y=0.13, yend=0.11), size=.4, 
               alpha=.4) +
  geom_segment(aes(x=3.8, xend=3.8, y=0.13, yend=0.11), size=.4, 
               alpha=.4) +
  geom_text(aes(x=4, y=.16, label="p-value= 0.008", family=""),
            size=4, family=my_font, color = "black") +
  ylim(-.2, .3) +
  coord_flip()


ggsave(filename=here("output", "no_na","conjoint_crime_victim_occup_quantile.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")




# Poisson Network: Police Violence ----------------------------------------------------

#model

mef_fs <- conjoint_data %>% 
  mutate(nvc=res_network_police_violence) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
       nvc*fs + fo, 
     data=., 
     clusters=responseid)

mod_pv <- mef_fs

mef_fo <- conjoint_data %>% 
  mutate(nvc=res_network_police_violence) %>%
  lm_robust(outcome_num ~ feat_political_party + feat_gender + 
       fs + nvc*fo, 
     data=., 
     clusters=responseid)


summary(mef_fs)
summary(mef_fo)

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

ggsave(filename=here("output", "no_na", "conjoint_police_violence.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


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



# Table -------------------------------------------------------------------

# Stargazer
library(stargazer)
library(modelsummary)
names_mod=names(coef(mod_cv))
names_modpv=names(coef(mod_pv))
new_names = c("(Intercept)" = "Intercept", 
              "feat_political_partyPRI" = "PRI",             
              "feat_political_partyMORENA" = "MORENA",
              "feat_political_partyPAN"   ="PAN",          
              "feat_genderMale"  = "Male",
              "nvc" = "Network Residuals (NR)",                                
              "fsEducation to Youth" =   "Education to Youth",
              "fsBetter Police/Security Cameras"    = "Better Police/Security Cameras",
              "fsDeath Penalty"   = "Death Penalty",           
              "fsPolice Militarization" = "Police Militarization",             
              "foLeader Autodefensas"  = "Leader Autodefensas",       
              "foChief Local Police"   =    "Chief Local Police",          
              "foHuman-Rights Activist" = "Human-Rights Activist",        
              "foOwner Private  Security Firm" = "Owner Private  Security Firm",      
              "nvc:fsEducation to Youth"  = "NR x Education to Youth",        
              "nvc:fsBetter Police/Security Cameras" = "NR x Better Police/Security Cameras",
              "nvc:fsDeath Penalty" = "NR x Death Penalty",               
              "nvc:fsPolice Militarization" = "NR x Police Militarization")


modelsummary(list("Crime Victimization"=mod_cv, 
                   "Police Victimization"=mod_pv), 
              fmt = 3,
              output = here("output", "no_na", "model_network_residuals_conjoint.tex"),
              stars = TRUE, 
              coef_map = new_names)




# Fear of Crime -----------------------------------------------------------
# Models: testing with nest_by


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

# ggsave(filename=here("output", "conjoint_fear_crime_policy.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# This is interesting: fear of crime increases support for death penalty.

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

# ggsave(filename=here("output", "no_na", "conjoint_fear_crime_ocup.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Law and Order -----------------------------------------------------------

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


# ggsave(filename=here("output", "no_na", "conjoint_laworder_preference_policy.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


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


# ggsave(filename=here("output", "no_na", "conjoint_laworder_preference_occup.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


ggplot(mef_pt, 
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


# ggsave(filename=here("output", "no_na", "conjoint_laworder_preference_party.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Trust In the Police -----------------------------------------------------

conjoint_data <- conjoint_data %>%
                  mutate_at(vars(trust_amlo, trust_police), ~as.numeric(.x)) %>%
                   mutate_at(vars(trust_amlo, trust_police), ~ ifelse(.x<0, NA,
                                                    .x))


lm(trust_police ~ income + 
     work + 
     as.numeric(age)+
     as.numeric(education) +
     gender +
     crime_victimization + 
     police_victimization  + vote_morena +vote_pri + vote_pan + as.numeric(lo_av) + trust_amlo , data=conjoint_data)%>%
  summary()

# Interesting that more punitive voters are less trustful in the police

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

# ggsave(filename=here("output", "no_na", "conjoint_trust_police_policy.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

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

# ggsave(filename=here("output", "no_na", "conjoint_trustpolice_occup.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

ggplot(mef_pt, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Trust Police", 
       caption="Baseline Condition: Independents") +
  facet_wrap(~treatment, ncol=2) 

# ggsave(filename=here("output", "no_na","conjoint_trust_police_party.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# Very interesting results. Voters who trust more in the police are less likely to 
# show stronger punitive attitudes. 

# Income ------------------------------------------------------------------

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

ggsave(filename=here("output", "no_na","conjoint_income_policy.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

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


# ggsave(filename=here("output", "no_na", "conjoint_income_occup.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

ggplot(mef_pt, 
       aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
  geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
  geom_ribbon(alpha=.1,color=NA, 
              linetype="dashed")  +
  geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
  labs(title="Marginal Component Interactive Effects", 
       y="Marginal Effect", 
       x="Income", 
       caption="Baseline Condition: Independent") +
  facet_wrap(~treatment, ncol=2) 


# ggsave(filename=here("output", "no_na","conjoint_income_party.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Crime Vitimization: Survey ------------------------------------------------------

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

# ggsave(filename=here("output", "no_na", "AMCE_gender.png") ,
#        width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Partisanship ------------------------------------------------------------


# By Party ----------------------------------------------------------------

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


# positive partisanship

ggplot(res_positive, aes(y=estimate, x=term, 
                        ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~subsample) +
  theme(axis.text.y=element_text(size=16,hjust=0))

# Negative Partisanship

ggplot(res_negative, aes(y=estimate, x=term, 
                         ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~subsample) +
  theme(axis.text.y=element_text(size=16,hjust=0))

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



# Positive
RColorBrewer::display.brewer.all()
ggplot(res_positive_r, aes(y=estimate, x=term_c, 
                         ymin=up, ymax=lb, fill=subsample)) +
  geom_pointrange(size=1.5, shape=21, color="grey5",
                  position=position_dodge(width = .8), alpha=.8) +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_fill_manual(values=c(pal[1],pal[9], "Green", pal[5]), 
                    name="")  +
coord_flip() + 
  facet_grid(features~subsample, scales = "free") +
  theme(axis.text.y=element_text(size=16))


# Negative Partisanship

ggplot(res_negative_r, aes(y=estimate, x=term_c, 
                         ymin=up, ymax=lb, fill=subsample)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="AMCE", 
       title = "Candidate Choice Conjoint Estimates") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_fill_manual(values=c(pal[1],pal[9], "Green"), 
                    name="")  +
  coord_flip() + 
  facet_grid(features~subsample, scales="free") +
  theme(axis.text.y=element_text(size=16,hjust=0))

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


ggsave(filename=here("output", "no_na", "positive_partisanship.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

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

ggsave(filename=here("output", "no_na","negative_partisanship.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# Interactive Effects By Features -----------------------------------------

lm(outcome_num ~ feat_security_proposal + feat_gender +
     feat_occupation + feat_political_party +
     feat_security_proposal*feat_political_party, data=conjoint_data) %>% tidy() %>% View()

res_int <- conjoint_data %>%
  nest_by(feat_political_party) %>% 
  drop_na() %>%
  mutate(model=map2(data, feat_political_party, 
         ~ lm_robust(outcome_num~ feat_security_proposal + feat_gender +
                          feat_occupation, 
                         data=.x, 
                     clusters = responseid) %>%
          tidy_conjoint_model(model=., subsample=.y, 
                            features = features,
                          references=references, list_terms = list_terms))) %>% 
  unnest(model) 


res_int_r <- reframe_graph(data=res_int, levels_gender, 
                                levels_party, levels_occup, levels_sec)  %>%
             drop_na() %>%
            filter(!str_detect(term, "Victims|Independents|Public"))



# ACIE for Party
levels(res_int_r$term)

res_int_r <- res_int_r %>%
                mutate(term=str_trim(str_replace_all(term, "/", " - \n")), 
                       term = fct_recode(term, "Education to  \n Youth"="Education to  Youth" , 
                                                "Police \n Militarization"="Police  Militarization"))

ggplot(data=res_int_r %>% filter(features=="Security Proposal"), 
       aes(y=estimate, x=subsample, 
                         ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Average Component Interactive Effects (ACIE)", 
       title = "Candidate Choice Conjoint Estimates", 
       caption="Reference Categories (Zero Red Line in the Graphs) is Victims Oriented Policies.") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~term) +
  theme(axis.text.y=element_text(size=16), 
        strip.text = element_text(size=13))


ggsave(filename=here("output", "no_na","party_acie_proposal.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# ACIE for Occupation
levels(res_int_r$term)

res_int_r<- res_int_r %>% 
            mutate(term= fct_recode(term,
                                    "Owner Private  - \n Security Firm" = "Owner Private  Security Firm", 
                                      "Human-Rights \n  Activist"="Human-Rights  Activist", 
                                      "Leader \n Autodefensas"="Leader Autodefensas", 
                                      "Chief \n Local Police" = "Chief Local Police"))

ggplot(data=res_int_r %>% filter(features=="Occupation"), 
       aes(y=estimate, x=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Average Component Interactive Effects (ACIE)", 
       title = "Candidate Choice Conjoint Estimates", 
       caption="Reference Categories (Zero Red Line in the Graphs) is Public Employee") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~term) +
  theme(axis.text.y=element_text(size=16), 
        strip.text = element_text(size=13))


ggsave(filename=here("output", "no_na","party_acie_occupation.png"),
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")


# Interactive Effects By Features -----------------------------------------

lm(outcome_num ~ feat_security_proposal + feat_gender +
     feat_occupation + feat_political_party*feat_occupation +
     feat_security_proposal*feat_occupation, data=conjoint_data) %>% tidy() %>% View()


# Occupation and Proposal

res_int <- conjoint_data %>%
  nest_by(feat_occupation) %>% 
  drop_na() %>%
  mutate(model=map2(data, feat_occupation, 
                    ~ lm_robust(outcome_num~ feat_security_proposal + feat_gender +
                                  feat_political_party, 
                                data=.x, 
                                clusters = responseid) %>%
                      tidy_conjoint_model(model=., subsample=.y, 
                                          features = features,
                                          references=references, list_terms = list_terms))) %>% 
  unnest(model) 


res_int_r <- reframe_graph(data=res_int, levels_gender, 
                           levels_party, levels_occup, levels_sec)  %>%
  drop_na() %>%
  filter(!str_detect(term, "Victims|Independents|Public"))

# ACIE for Proposal 

res_int_r <- res_int_r %>%
  mutate(term=str_trim(str_replace_all(term, "/", " - \n")), 
         term = fct_recode(term, "Education to  \n Youth"="Education to  Youth" , 
                           "Police \n Militarization"="Police  Militarization"))

ggplot(data=res_int_r %>% filter(features=="Security Proposal"), 
       aes(y=estimate, x=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Average Component Interactive Effects (ACIE)", 
       title = "Candidate Choice Conjoint Estimates", 
       caption="Reference Categories (Zero Red Line in the Graphs) is Victims Oriented Policies.") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~term) +
  theme(axis.text.y=element_text(size=14), 
        strip.text = element_text(size=13))


ggsave(filename=here("output", "no_na", "occup_acie_proposal.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")



# ACIE for Occupation and Party

ggplot(data=res_int_r %>% filter(features=="Party"), 
       aes(y=estimate, x=subsample, 
           ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Average Component Interactive Effects (ACIE)", 
       title = "Candidate Choice Conjoint Estimates", 
       caption="Reference Categories (Zero Red Line in the Graphs) is Public Employee") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~term) +
  theme(axis.text.y=element_text(size=16), 
        strip.text = element_text(size=13))




# Leeper Marginal Mean ----------------------------------------------------
install.packages("cregg")
library("cregg")


data("immigration")
data("taxes")


# descriptive plotting
f1 <- ChosenImmigrant ~ Gender + Education + LanguageSkills + CountryOfOrigin + Job + JobExperience + JobPlans + ReasonForApplication + 
  PriorEntry
plot(mm(immigration, f1, id = ~CaseID), vline = 0.5)


immigration$ChosenImmigrant


# Findit ------------------------------------------------------------------

library(FindIt)

conjoint_findit <- select(conjoint_data, contains("feat"), 
                          outcome_num, responseid) %>%
                    drop_na()

# weird the package was not working when relevel within the pipes
conjoint_findit$feat_gender<- factor(conjoint_findit$feat_gender,ordered=TRUE,
                            levels=c("Male", "Female"))
conjoint_findit$feat_political_party<- factor(conjoint_findit$feat_political_party,ordered=TRUE,
                                     levels=c("PAN", "PRI",
                                      "MORENA", "Independents"))

conjoint_findit$feat_security_proposal<- factor(conjoint_findit$feat_security_proposal,
                                                ordered=TRUE,
                                     levels=c("Police Militarization" ,
                                              "Education to Youth" ,           
                                              "Better Police/Security Cameras", 
                                              "Death Penalty",                 
                                              "Victims Oriented Policies"))
conjoint_findit$feat_occupation<- fct_rev(conjoint_findit$feat_occupation)

levels(conjoint_findit$feat_gender)

# Sample for training and test observations
train.ind <- sample(unique(conjoint_findit$responseid), 
                    length(unique(conjoint_findit$responseid)) / 2 - 0.5, 
                    replace = FALSE)

test.ind <- setdiff(unique(conjoint_findit$responseid), train.ind)


# Assign train and test data objects
train <- conjoint_findit[is.element(conjoint_findit$responseid,train.ind),]
test <-  conjoint_findit[is.element(conjoint_findit$responseid,test.ind),]

# Simple estimation first

fit1 <- CausalANOVA(formula=outcome_num~ feat_political_party + feat_gender + 
                      feat_security_proposal + feat_occupation ,
                    data=conjoint_findit, pair.id=conjoint_findit$responseid,
                    diff=TRUE,
                    cluster=conjoint_findit$responseid, nway=1)


#################### AMEs and two-way AMIEs ####################

fit2 <- CausalANOVA(formula=outcome_num~ feat_political_party + feat_gender + 
                      feat_security_proposal + feat_occupation ,
                    data=conjoint_findit, pair.id=conjoint_findit$responseid,
                    diff=TRUE,
                    cluster=conjoint_findit$responseid, nway=2)

View(summary(fit2)[[4]])

# AMIE: Party x Occupation
list_party <- ConditionalEffect(fit2, cond.fac="feat_political_party", 
                  treat.fac="feat_occupation")$ConditionalEffects

as.data.frame(list_party[[1]])

res_party_occ <- map2_df(list_party, names(list_party), ~
         as.data.frame(.x) %>%
         set_names(., nm=c("AMIE", "sd", "lb", "up")) %>%
          mutate(condition=str_remove_all(.y, "feat_political_party="), 
                 treat=row.names(.)))


# AMIE: Party x Proposal

list_party_prop<- ConditionalEffect(fit2, cond.fac="feat_political_party", 
                                treat.fac="feat_security_proposal")$ConditionalEffects

res_party_prop <- map2_df(list_party_prop, names(list_party), ~
                           as.data.frame(.x) %>%
                           set_names(., nm=c("AMIE", "sd", "lb", "up")) %>%
                           mutate(condition=str_remove_all(.y, "feat_political_party="), 
                                  treat=row.names(.)))

# AMIE: Occu x Proposal

list_party <- ConditionalEffect(fit2, cond.fac="feat_security_proposal", 
                                treat.fac="feat_occupation")$ConditionalEffects

res_party_occ <- map2_df(list_party, names(list_party), ~
                           as.tibble(.x) %>%
                           set_names(., nm=c("AMIE", "sd", "lb", "up")) %>%
                           mutate(condition=str_remove_all(.y, "feat_security_proposal=")))


# results are pretty much the same as the liner interactions

# Causal anova, screening for interactions and collapsing -----------------

# Estimate AMCEs

ca1.r <- CausalANOVA(formula=outcome_num~ feat_political_party + feat_gender + 
                       feat_security_proposal + feat_occupation ,
                     # Training data
                     data = train, 
                     cluster = train$respno, 
                     # Allow for up to 3 way interactions
                     nway = 2,
                     diff = T, 
                     pair.id = train$responseid,
                     # Screen for interactions
                     screen = T, 
                     # Allow collapsing of factors within groups
                     collapse = TRUE, 
                     seed = 23)

# Inference (i.e. get some SEs)
ca1.rt <- test.CausalANOVA(ca1.r, 
                           # Test data
                           newdata = test, 
                           diff = TRUE,
                           pair.id=test$responseid, 
                           cluster=test$responseid)


ca1.rt$CI.table[[1]]
ca1.rt$CI.table[[2]]
ca1.rt$CI.table[[3]]
ca1.rt$CI.table[[4]]


# # Network Models ----------------------------------------------------------
# 
# 
# network_size <- conjoint_data  %>% 
#   select(responseid, starts_with("network"), 
#          -contains("click"), -contains("submit")) %>%
#   distinct() %>%
#   rowwise() %>%
#   mutate(mean_network=mean(c_across(starts_with("network")), na.rm = T), 
#          sum_network=sum(c_across(starts_with("network")), na.rm = T)) %>%
#   select(responseid, size_network, sum_network) %>%
#   
#   
#   conjoint_data <- left_join(conjoint_data, network_size)  %>%
#   mutate(c_sh_net = network_victim_crime/sum_network, 
#          c_mean_net = network_victim_crime/size_network)
# 
# 
# 
# # share
# 
# conjoint_data <- conjoint_data %>%
#   mutate(c_sh_net = network_victim_crime/sum_network, 
#          c_mean_net = network_victim_crime/size_network)
# 
# #model
# 
# mef_fs <- conjoint_data %>% 
#   mutate(cv=crime_victimization, nvc=network_victim_crime) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        nvc*fs + fo, 
#      data=.)
# 
# mef_fo <- conjoint_data %>% 
#   mutate(cv=crime_victimization, nvc=network_victim_crime) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        fs + nvc*fo, 
#      data=.)
# 
# # Marginal Effects
# 
# 
# mef_fs %>% tidy() %>% pull(term)
# mef_fo %>% tidy() %>% pull(term)
# 
# # Get names: Constitutive Terms
# list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
#                    "fsDeath Penalty", "fsPolice Militarization")
# list_interactive_fs <- as.list(paste0("nvc:", list_ct_fs))
# 
# list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
#                    "foHuman-Rights Activist", "foOwner Private  Security Firm")
# 
# list_interactive_fo <- as.list(paste0("nvc:", list_ct_fo))
# 
# interaction_plot_continuous_ggplot()
# 
# 
# # Mef FS
# interaction_plot_continuous_ggplot(
#   mef_fs, 
#   effect = list_ct_fs[[1]], 
#   moderator = "nvc", 
#   interaction =list_interactive[[1]], 
#   num_points = 11)
# 
# mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_fs, 
#                       effect = .x, 
#                       moderator = "nvc", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fs")))
# 
# # fo
# 
# mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_fo, 
#                       effect = .x, 
#                       moderator = "nvc", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fo")))
# 
# 
# # Plot Marginal Effects Interactive
# 
# 
# ggplot(mef_fs, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Security Policy Proposal", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# 
# 
# # Network Relative --------------------------------------------------------
# 
# mef_share_crime_fs <- conjoint_data %>% 
#   mutate(cv=crime_victimization, nvc=c_sh_net) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        nvc*fs + fo, 
#      data=.)
# 
# mef_share_crime_fo <-conjoint_data %>% 
#   mutate(cv=crime_victimization, nvc=c_sh_net) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        fs + nvc*fo, 
#      data=.)
# 
# # Marginal Effects
# 
# # Trust invest
# 
# mef_share_crime %>% tidy() %>% pull(term)
# mef_share_crime %>% tidy() %>% pull(term)
# 
# # Get names: Constitutive Terms
# list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
#                    "fsDeath Penalty", "fsPolice Militarization")
# list_interactive_fs <- as.list(paste0("nvc:", list_ct_fs))
# 
# list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
#                    "foHuman-Rights Activist", "foOwner Private  Security Firm")
# 
# list_interactive_fo <- as.list(paste0("nvc:", list_ct_fo))
# 
# # Mef Share Network FS
# 
# mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_share_crime_fs, 
#                       effect = .x, 
#                       moderator = "nvc", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fs")))
# 
# # fo
# 
# mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_share_crime_fo, 
#                       effect = .x, 
#                       moderator = "nvc", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fo")))
# 
# 
# # Plot Marginal Effects Interactive
# 
# ggplot(mef_fs, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Exposure to Crime in the Personal Network", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# ggplot(mef_fo, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Exposure to Crime in the Personal Network", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# 
# 
# # Variation to the mean ---------------------------------------------------
# 
# plot(density(conjoint_data$c_mean_net, na.rm = TRUE))
# 
# mef_mean_crime_fs <- conjoint_data %>% 
#   mutate(cv=crime_victimization, nvc=c_mean_net) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        nvc*fs + fo, 
#      data=.)
# 
# summary(mef_mean_crime_fs)
# 
# mef_mean_crime_fo <-conjoint_data %>% 
#   mutate(cv=crime_victimization, nvc=c_mean_net) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        fs + nvc*fo, 
#      data=.)
# 
# summary(mef_mean_crime_fo)
# 
# 
# # Marginal Effects
# 
# # Trust invest
# 
# mef_share_crime_fs %>% tidy() %>% pull(term)
# mef_share_crime %>% tidy() %>% pull(term)
# 
# # Get names: Constitutive Terms
# list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
#                    "fsDeath Penalty", "fsPolice Militarization")
# list_interactive_fs <- as.list(paste0("nvc:", list_ct_fs))
# 
# list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
#                    "foHuman-Rights Activist", "foOwner Private  Security Firm")
# 
# list_interactive_fo <- as.list(paste0("nvc:", list_ct_fo))
# 
# # Mef Share Network FS
# 
# mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_mean_crime_fs, 
#                       effect = .x, 
#                       moderator = "nvc", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fs")))
# 
# # fo
# 
# mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_mean_crime_fo, 
#                       effect = .x, 
#                       moderator = "nvc", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fo")))
# 
# 
# # Plot Marginal Effects Interactive
# 
# ggplot(mef_fs, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Exposure to Crime in the Personal Network", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# ggplot(mef_fo, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Exposure to Crime in the Personal Network", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# dev.off()
# 
# 
# 
# 
# 
# # Model
# 
# res_vict <- d_vict %>% 
#   group_by(police_victimization) %>% 
#   nest() %>% 
#   drop_na() %>%
#   mutate(model=map2(data, police_victimization, 
#                     ~ lm(outcome_num~ feat_political_party + feat_gender + 
#                            feat_security_proposal + feat_occupation, 
#                          data=.x) %>%
#                       tidy_conjoint_model(model=., subsample=.y, 
#                                           features = features,
#                                           references=references, list_terms = list_terms))) %>% 
#   unnest(model) %>% select(-data)
# 
# 
# 
# # Remove repeated
# 
# res_vict_color <- res_vict %>% ungroup() %>% 
#   mutate(id_remove=ifelse(police_victimization=="Victim"& (is.na(estimate) | estimate==0.0000), 1, 0)) %>%
#   filter(id_remove==0) 
# 
# 
# ggplot(res_vict_color, aes(y=estimate, x=term, 
#                            ymin=up, ymax=lb, shape=police_victimization)) +
#   geom_pointrange(size=1, fill="black", color="grey5",
#                   position=position_dodge(width = .7), alpha=.8) +
#   labs(x="", y="AMCE", 
#        title = "Candidate Choice Conjoint Estimates") +
#   geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
#   scale_shape_manual(values=c(22,23), name="Police Victimization") +
#   coord_flip() + 
#   theme(axis.text.y=element_text(size=16,hjust=0)) +
#   guides(shape = guide_legend(reverse=T))
# 
# 
# 
# # Network Police Violence -------------------------------------------------
# 
# # share
# 
# conjoint_data <- conjoint_data %>%
#   mutate(c_sh_net_pv = network_police_violence/sum_network, 
#          c_mean_net_pv = network_police_violence/size_network)
# 
# 
# plot(density(conjoint_data$network_police_violence, na.rm = TRUE))
# 
# #model
# 
# mef_fs_pv <- conjoint_data %>% 
#   mutate(pv=network_police_violence) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        pv*fs + fo, 
#      data=.)
# 
# mef_fo_pv <- conjoint_data %>% 
#   mutate(pv=network_police_violence) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        fs + pv*fo, 
#      data=.)
# 
# # Marginal Effects
# mef_fs_pv %>% tidy() %>% pull(term)
# mef_fo_pv %>% tidy() %>% pull(term)
# 
# # Get names: Constitutive Terms
# list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
#                    "fsDeath Penalty", "fsPolice Militarization")
# list_interactive_fs <- as.list(paste0("pv:", list_ct_fs))
# 
# list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
#                    "foHuman-Rights Activist", "foOwner Private  Security Firm")
# 
# list_interactive_fo <- as.list(paste0("pv:", list_ct_fo))
# 
# 
# # Mef FS
# 
# mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_fs_pv, 
#                       effect = .x, 
#                       moderator = "pv", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fs")))
# 
# # fo
# 
# mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_fo_pv, 
#                       effect = .x, 
#                       moderator = "pv", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fo")))
# 
# 
# # Plot Marginal Effects Interactive
# ggplot(mef_fs, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Number of People you know who where victim of Police Violence", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# ggplot(mef_fo, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Number of People you know who where victim of Police Violence", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# 
# # Network Relative --------------------------------------------------------
# 
# plot(density(conjoint_data$c_sh_net_pv, na.rm=TRUE))
# plot(density(conjoint_data$network_police_violence, na.rm=TRUE))
# 
# #model
# 
# mef_fs_pv <- conjoint_data %>% 
#   mutate(pv=c_mean_net_pv) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        pv*fs + fo, 
#      data=.)
# 
# tidy(mef_fs_pv)
# 
# mef_fo_pv <- conjoint_data %>% 
#   mutate(pv=c_mean_net_pv) %>%
#   lm(outcome_num ~ feat_political_party + feat_gender + 
#        fs + pv*fo, 
#      data=.)
# 
# tidy(mef_fo_pv)
# 
# # Marginal Effects
# mef_fs_pv %>% tidy() %>% pull(term)
# mef_fo_pv %>% tidy() %>% pull(term)
# 
# # Get names: Constitutive Terms
# list_ct_fs <- list("fsEducation to Youth", "fsBetter Police/Security Cameras",
#                    "fsDeath Penalty", "fsPolice Militarization")
# 
# list_interactive_fs <- as.list(paste0("pv:", list_ct_fs))
# 
# list_ct_fo <- list("foLeader Autodefensas", "foChief Local Police",
#                    "foHuman-Rights Activist", "foOwner Private  Security Firm")
# 
# list_interactive_fo <- as.list(paste0("pv:", list_ct_fo))
# 
# 
# # Mef FS
# 
# mef_fs <- map2_df(list_ct_fs, list_interactive_fs, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_fs_pv, 
#                       effect = .x, 
#                       moderator = "pv", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fs")))
# 
# # fo
# 
# mef_fo <- map2_df(list_ct_fo, list_interactive_fo, ~ 
#                     interaction_plot_continuous_ggplot(
#                       mef_fo_pv, 
#                       effect = .x, 
#                       moderator = "pv", 
#                       interaction = .y, 
#                       num_points = 11) %>%
#                     mutate(treatment=.x)) %>%
#   mutate(treatment=fct_inorder(str_remove(treatment, "fo")))
# 
# 
# # Plot Marginal Effects Interactive
# ggplot(mef_fs, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Number of People you know who where victim of Police Violence", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# ggplot(mef_fo, 
#        aes(y=delta_1, ymax=upper_bound, ymin=lower_bound, x=moderator)) +
#   geom_pointrange(shape=21, size=1.5, fill="white", color="gray70") +
#   geom_ribbon(alpha=.1,color=NA, 
#               linetype="dashed")  +
#   geom_hline(aes(yintercept=0), linetype="dashed", color="tomato2") +
#   labs(title="Marginal Component Interactive Effects", 
#        y="Marginal Effect", 
#        x="Number of People you know who where victim of Police Violence", 
#        caption="Baseline Condition: Candidate Proposing Victims Oriented Policies ") +
#   facet_wrap(~treatment, ncol=2) 
# 
# 
