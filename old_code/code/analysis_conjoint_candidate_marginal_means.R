# ------------------------------------------------------------------------------- #
# Article: Voting for Law and Order in Mexico
# Authors: Cantu, Ley, Ventura
# Update: June 15
# ------------------------------------------------------------------------------- #

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
library(cregg)
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

d <- read_csv(here("data", "data_conjoint.csv"))
dim(d)


# Rename levels -----------------------------------------------------------

# Gender
levels_gender <- levels(as.factor(d$feat_gender))
levels_gender <- list("Male"= levels_gender[2], 
                      "Female" = levels_gender[1])

# Party

levels_party <- levels(d$feat_political_party)
levels_party <- list("PRI"="PRI", "MORENA"="Morena", "PAN"="PAN", 
                     "Independents"="Independiente")

# Occupation
levels_occup <- levels(as.factor(d$feat_occupation))
levels_occup <- list("Leader Autodefensas" = levels_occup[3], 
                     "Chief Local Police" = levels_occup[1], 
                     "Human-Rights Activist"=levels_occup[2], 
                     "Owner Private Security Firm"=levels_occup[4], 
                     "Public Employee"=levels_occup[5])

# Proposal
levels_sec <- levels(as.factor(d$feat_security_proposal))
levels_sec <- list("Education to Youth"= levels_sec[3], 
                   "Better Police/Security Cameras"= levels_sec[1],
                   "Death Penalty"= levels_sec[2],
                   "Police Militarization"= levels_sec[4], 
                   "Victims Oriented Policies"= levels_sec[5])
d <- d %>% 
  mutate(feat_gender=fct_recode(feat_gender, !!!levels_gender), 
         feat_political_party=fct_recode(feat_political_party, !!!levels_party), 
         feat_occupation=fct_rev(fct_recode(feat_occupation, !!!levels_occup)), 
         feat_security_proposal=fct_rev(fct_recode(feat_security_proposal,
                                           !!!levels_sec))) 
levels(d$feat_occupation)

features = c("political_party", "gender", "occupation", "security_proposal")
references <- c(levels(d$feat_political_party)[1],
                levels(d$feat_gender)[1],
                levels(d$feat_occupation)[1], 
                levels(d$feat_security_proposal)[1])

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

# ATE

library(estimatr)

model <- lm_robust(outcome_num~ feat_political_party + feat_gender + 
                     feat_security_proposal + feat_occupation, 
                   data=d, 
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



# Leeper Model ------------------------------------------------------------


## AMCE

model <- cj(d, 
   outcome_num ~ feat_political_party + feat_gender + 
     feat_security_proposal + feat_occupation, 
   id= ~responseid, 
   estimate="amce")

# Clean to get the same graph
model_clean <- model %>%
  select(term=level, estimate, std.error, statistic=z, p.value=p, 
         lower, upper) %>%
  mutate(term=str_c("    ", term)) %>%
  bind_rows(data_frame(term=c(str_to_title(str_replace_all(features, "_", " ")), 
                              " ", "  ", "   ", "    "), 
                       estimate=c(rep(NA, 4), rep(NA, 4)),  
                       std.error=c(rep(NA, 4), rep(NA, 4)),  
                       statistic=c(rep(NA, 4), rep(NA, 4)),  
                       p.value=c(rep(NA, 4), rep(NA, 4)), 
                       lower=c(rep(NA, 4), rep(NA, 4)),  
                       upper=c(rep(NA, 4), rep(NA, 4))))  %>% 
  mutate(term=fct_relevel(term, list_terms), 
         term=fct_rev(term), 
         title="AMCE From Leeper") 

ggplot(model_clean, aes(y=estimate, x=term, 
                ymin=upper, ymax=lower)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
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


## Marginal Means


model <- cj(d, 
            outcome_num ~ feat_political_party + feat_gender + 
              feat_security_proposal + feat_occupation, 
            id= ~responseid, 
            estimate="mm")

# Clean to get the same graph
model_clean <- model %>%
  select(term=level, estimate, std.error, statistic=z, p.value=p, 
         lower, upper) %>%
  mutate(term=str_c("    ", term)) %>%
  bind_rows(data_frame(term=c(str_to_title(str_replace_all(features, "_", " ")), 
                              " ", "  ", "   ", "    "), 
                       estimate=c(rep(NA, 4), rep(NA, 4)),  
                       std.error=c(rep(NA, 4), rep(NA, 4)),  
                       statistic=c(rep(NA, 4), rep(NA, 4)),  
                       p.value=c(rep(NA, 4), rep(NA, 4)), 
                       lower=c(rep(NA, 4), rep(NA, 4)),  
                       upper=c(rep(NA, 4), rep(NA, 4))))  %>% 
  mutate(term=fct_relevel(term, list_terms), 
         term=fct_rev(term), 
         title="Marginal Means") 

ggplot(model_clean, aes(y=estimate, x=term, 
                        ymin=upper, ymax=lower)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  labs(x="", y="Point Estimates", 
       title = "") +
  geom_hline(yintercept = .5, color="darkred", linetype="twodash", alpha=.6) + 
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



# Victimization Survey ----------------------------------------------------
amce_diagnostic <- amce_by_reference(d, outcome_num ~  feat_occupation, ~ feat_occupation, 
                  id= ~responseid)
plot(amce_diagnostic, group = "REFERENCE", legend_title = "Reference Category")

# Crime Vitimization: Survey ------------------------------------------------------
d$crime_victimization <- as.factor(d$crime_victimization)

# crime victimization
mms <- cj(d, 
          outcome_num ~ feat_political_party + feat_gender + 
            feat_security_proposal + feat_occupation, 
          id= ~responseid, 
          estimate = "mm_diff", 
          by = ~crime_victimization)
plot(mms) + 
  facet_wrap(~BY)


# Police Violence: Survey -------------------------------------------------
d$police_victimization <- as.factor(d$police_victimization)

mms <- cj(d, 
          outcome_num ~ feat_political_party + feat_gender + 
            feat_security_proposal + feat_occupation, 
          id= ~responseid, 
          estimate = "mm_diff", 
          by = ~police_victimization)

plot(mms) + 
  facet_wrap(~BY)



# From poisson ------------------------------------------------------------
c_t <- d %>%
  ungroup() %>%
  mutate(quant=ntile(res_network_victim_crime, 3),
         quant_1_5=case_when(quant=="1" ~ "2sd_minus",
                             quant=="3" ~ "2sd_plus"), 
         quant_1_5 = as_factor(quant_1_5))


mms <- cj(c_t, 
          outcome_num ~ feat_political_party + feat_gender + 
            feat_security_proposal + feat_occupation, 
          id= ~responseid, 
          estimate = "mm_diff", 
          by = ~quant_1_5) 
plot(mms) + 
  facet_wrap(~BY)


c_t_pol <- d %>%
  ungroup() %>%
  mutate(quant=ntile(res_network_police_violence, 5),
         quant_1_5=case_when(quant=="1" ~ "2sd_minus",
                             quant=="5" ~ "2sd_plus"), 
         quant_1_5 = as_factor(quant_1_5))


mms <- cj(c_t_pol, 
          outcome_num ~ feat_political_party + feat_gender + 
            feat_security_proposal + feat_occupation, 
          id= ~responseid, 
          estimate = "mm_diff", 
          by = ~quant_1_5) 
plot(mms) + 
  facet_wrap(~BY)

# From Network Mean ------------------------------------------------------------
network_questions <- c("network_silvia", "network_antonio","network_police", 
                        "network_medicos",      "network_hijo", 
                        "network_died" ,  "network_married", "network_covid", 
                        "network_public_employee"     ,   "network_social_work"    ,
                        "network_partisan", "network_candidate" , 
                        "network_victim_crime", "network_police_violence" , "network_prision" , 
                        "network_job_covid")

d_net <- d %>% select(responseid, network_questions, outcome_num, contains("feat"))


d_net <- d_net %>%
  rowwise() %>%
  mutate(m=mean(c_across(network_silvia:network_job_covid), na.rm = TRUE )) %>% 
  mutate(crime_dif=network_victim_crime-m, 
         pol_dif=network_police_violence-m)


# with AMCE
model <- lm_robust(outcome_num~ feat_political_party + feat_gender + 
                     feat_security_proposal*crime_dif + feat_occupation, 
                   data=d_net, 
                   clusters = responseid,
                   se_type = "stata") 

c_t <- d_net %>%
  ungroup() %>%
  mutate(quant=ntile(crime_dif, 5),
         quant_1_5=case_when(quant=="1" ~ "2sd_minus",
                             quant=="5" ~ "2sd_plus"), 
         quant_1_5 = as_factor(quant_1_5))


mms <- cj(c_t, 
          outcome_num ~ feat_political_party + feat_gender + 
            feat_security_proposal + feat_occupation, 
          id= ~responseid, 
          estimate = "mm_diff", 
          by = ~quant_1_5) 

plot(mms) + 
  facet_wrap(~BY)


# Partisanship ------------------------------------------------------------

d$vote_morena <- as.factor(d$vote_morena)

# crime victimization
mms_morena <- cj(d, 
          outcome_num ~ feat_political_party + feat_gender + 
            feat_security_proposal + feat_occupation, 
          id= ~responseid, 
          estimate = "mm_diff", 
          by = ~vote_morena)

plot(mms_morena) + 
  facet_wrap(~BY)

d$vote_pan <- as.factor(d$vote_pan)

# PAN
mms_pan <- cj(d, 
                 outcome_num ~ feat_political_party + feat_gender + 
                   feat_security_proposal + feat_occupation, 
                 id= ~responseid, 
                 estimate = "mm_diff", 
                 by = ~vote_pan)

plot(mms_pan) + 
  facet_wrap(~BY)


# Punitive Features -------------------------------------------------------
levels(d$feat_security_proposal)

d_ <- d %>% 
      drop_na(feat_security_proposal) %>%
      mutate(punitive=ifelse(feat_security_proposal=="Death Penalty" | feat_security_proposal=="Death Penalty",
                             "Punitive" ,
                             "Non Punitive"),
             punitive=as_factor(punitive), 
             punitive=fct_relevel(punitive, "Non Punitive"))



mms_pun <- cj(d_, 
              outcome_num ~ feat_political_party + feat_gender + 
                feat_occupation, 
              id= ~responseid, 
              estimate = "mm_diff", 
              by = ~punitive)

plot(mms_pun) + 
  facet_wrap(~BY)
