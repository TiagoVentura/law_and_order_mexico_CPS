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

#load("./data/mexico_cleaned2_data.Rdata")
d <- read_csv("data_sample_lucia.csv")

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

# The features ------------------------------------------------------------

### Clean the features


### Function to extract the features

features <- d %>% select(responseid, matches("^f_"))
View(features)

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
conjoint_data %>% tabyl(feat_security_proposal) %>% View()
conjoint_data %>% tabyl(feat_occupation) %>% View()







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
                     "Human-Rights  Activist"=levels_occup[4], 
                     "Owner Private  Security Firm"=levels_occup[5], 
                     "Public Employee"=levels_occup[1])

# Proposal
levels_sec <- levels(conjoint_data$feat_security_proposal)
levels_sec <- list("Education to  Youth"= levels_sec[2], 
                   "Better Police/Security Cameras"= levels_sec[3],
                   "Death Penalty"= levels_sec[4],
                   "Police  Militarization"= levels_sec[5], 
                   "Victims Oriented  Policies"= levels_sec[1])

conjoint_data <- conjoint_data %>% 
  mutate(feat_gender=fct_recode(feat_gender, !!!levels_gender), 
         feat_political_party=fct_recode(feat_political_party, !!!levels_party), 
         feat_occupation=fct_recode(feat_occupation, !!!levels_occup), 
         feat_security_proposal=fct_recode(feat_security_proposal,
                                           !!!levels_sec))


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

## Function to get the results
# this is where I basically prepare the output to be ready for the plot. 

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
