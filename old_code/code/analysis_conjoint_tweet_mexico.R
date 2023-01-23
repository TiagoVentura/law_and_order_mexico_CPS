# ------------------------------------------------------------------------------- #
# Conjoint Tweet Experiment in Brazil
# ------------------------------------------------------------------------------- #

# Basics ------------------------------------------------------------------
rm(list=ls(all=TRUE))
path_tiago <- "C:/Users/venturat/Dropbox/artigos/ACSV_Trustful_voters" 
setwd(path_tiago)
#setwd(path_ernesto)

# Packages ----------------------------------------------------------------
library(tidyverse)
library(summarytools)
library(lubridate)
library(scales)
library(qualtRics)
library(conflicted)
library(rebus)
library(patchwork)
library(extrafont)
library(broom)
#devtools::install_github("sfirke/janitor")
#library(janitor)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# Functions ---------------------------------------------------------------


# short view
short_view <- function(data){
  data %>% slice(1:50) %>% View()
}

# ggplot theme ------------------------------------------------------------
RColorBrewer::display.brewer.all()
my_font <- "Palatino Linotype"
my_bkgd <- "white"
#my_bkgd <- "#f5f5f2"
pal <- RColorBrewer::brewer.pal(9, "Spectral")


my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_rect(color="black"), 
                  strip.background = element_rect(color="black", fill="gray85"), 
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 6, fill = "white", colour = NA), 
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 14, family = my_font),
                  legend.title = element_text(size=14),
                  plot.title = element_text(size = 22, face = "bold", family=my_font),
                  plot.subtitle = element_text(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  
                  axis.text = element_text(size=14, family=my_font),
                  axis.title.x = element_text(hjust=1),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 14, face="bold"), 
                  plot.margin = margin(1, 1, 1, 1, "cm"))
theme_set(theme_bw() + my_theme)



# Download the data -------------------------------------------------------

load("./data/mexico_cleaned2_data.Rdata")

dim(d)

# Check Covariates --------------------------------------------------------

levels(d$education)
class(d$income)
levels(d$work)
levels(d$age)
d$income_assistance


# Conjoint Models ---------------------------------------------------------


d_conj_outcomes <- d %>% select(responseid, conjoint_share, conjoint_not_share, 
                                conjoint_friends_exposure, conjoint_news_exposure) 


d_conj_features <- d %>% select(responseid, contains("header"), 
                                contains("text_"), 
                                contains("picture_"), 
                                contains("foot_")) 


# Clean Conjoint ----------------------------------------------------------


# Header: 2 Features : 4 combinations

unique(d_conj_features$header_tweet_1)
unique(d_conj_features$header_tweet_2)

relevel_header <- list("Conservative_media" = "https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_6FENsMoAG6YX5lj", 
                       "Liberal_Media"= "https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_8G78xysVvLzt9Wd")



#Text: 
#map(unique(d_conj_features$text_tweet_1), browseURL)
#map(unique(d_conj_features$text_tweet_2), browseURL)


relevel_text <- list("Anti_Government"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_eQeKBewaLRL5Vl3", 
                            "Anti_Government"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_bBnIcMD9v3D5Mcl", 
                            "Neutral"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_29LthA2M52A7xm5", 
                            "Neutral"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_eKU7uGykLfSMyzz")

#map(unique(d_conj_features$picture_tweet_1), browseURL)
#map(unique(d_conj_features$picture_tweet_2), browseURL)

# Image

relevel_image <- list("Kids"= "https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_1Mt9rwFdouuu0tL", 
                      "Militarization"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_3luYbhVP14Ubr0h", 
                      "Neutral"= "https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_77q5ZY28ObShMd7")



# Support

#map(unique(d_conj_features$foot_tweet_1), browseURL)
#map(unique(d_conj_features$foot_tweet_2), browseURL)

relevel_support <- list("High_Support"= "https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_3fMM9Of2A4PqSBT", 
                      "Low_Support"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_8FY2AVO5zverwcl", 
                      "Low_Support"= "https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_3K0RHPC1jDZ12rH", 
                      "High_Support"="https://umdsurvey.umd.edu/ControlPanel/Graphic.php?IM=IM_3DeCxQpqwMCUGDH") 


# Fix names

colnames(d_conj_features)

d_conj_features <- d_conj_features %>% 
  mutate_at(vars(header_tweet_1, header_tweet_2), ~
              fct_recode(.x, !!!relevel_header)) %>%
  mutate_at(vars(text_tweet_1, text_tweet_2),
            ~ fct_recode(.x, !!!relevel_text)) %>%
  mutate_at(vars(foot_tweet_1, foot_tweet_2),
            ~ fct_recode(.x, !!!relevel_support)) %>%
  mutate_at(vars(picture_tweet_1, picture_tweet_2), ~
              fct_recode(.x, !!!relevel_image))

# check if it is working
d_conj_features %>% map(levels)


### Function to extract the features
data = d_conj_features

# Function
cj_extract_features <- function(data, variables, new_var){
  feature <- data %>% select(responseid,variables)
  feature %>% pivot_longer(
    cols = variables,
    names_to = "var",
    values_to = new_var) %>% 
    mutate(tweet= ifelse(str_detect(var, "tweet" %R% "_1" %R% END),"tuite_1", "tuite_2")) %>%
    select(responseid, new_var, tweet)  
}


## Extract

list_features <- list(c("header_tweet_1", "header_tweet_2"),
                      c("text_tweet_1", "text_tweet_2"),
                      c("picture_tweet_1", "picture_tweet_2"), 
                      c("foot_tweet_1", "foot_tweet_2"))

list_names <- list("header", "text", "picture", "foot")

features_long <- map2_dfc(list_features, list_names, ~ cj_extract_features(d_conj_features, .x, .y)) %>%
  select(responseid, tweet, !!!list_names)

View(features_long)

# Merge back with outcomes ------------------------------------------------

d_conjoint <- left_join(d, features_long)


d_conjoint$conjoint_not_share

# Recode dv

d_conjoint <- d_conjoint %>% 
  mutate(rec_conjoint_share=ifelse((conjoint_share=="Tweet 1" & 
                                      tweet=="tuite_1") | 
                                     (conjoint_share=="Tweet 2" & 
                                        tweet=="tuite_2") | conjoint_share=="Ambos", 1, 0), 
         rec_conjoint_not_share=ifelse((conjoint_not_share=="Tuíte 1" & 
                                          tweet=="tuite_1") | 
                                         (conjoint_not_share=="Tuíte 2" & 
                                            tweet=="tuite_2"), 1, 0)) 



# Check here
table(d$conjoint_share)
table(d_conjoint$rec_conjoint_share)

287*2 + 546 + 485 # perfect twee1 + tweet2 + 2*ambos

# Perfect


# Modelling ---------------------------------------------------------------

tidy_model <- function(model, subsample, number_features=4){
  # to make it more general, it is just to change the  4 for the number of features
  model %>% 
    tidy(.) %>% 
    mutate(lb=estimate - 1.96*std.error, 
           up= estimate + 1.96*std.error, 
           lb90=estimate - 1.64*std.error, 
           up90= estimate + 1.64*std.error) %>% 
    filter(!(term %in% c("(Intercept)"))) %>% 
    mutate(term=str_remove_all(term, "text|foot|picture|header"), 
           term=str_replace_all(term, "_", " "), 
           term=str_c("    ", term)) %>%
    bind_rows(data_frame(term=c("Header:", "Text:", "Picture:", "Foot:", 
                                " ", "  ", "   ", "    ",
                                "    Neutral Text", "    Neutral Image", "    Low Support", 
                                "    Conservative Media"), 
                         estimate=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         std.error=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         statistic=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         p.value=c(rep(NA, 4), rep(NA, 4), rep(0,4)), 
                         lb=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         up=c(rep(NA, 4), rep(NA, 4), rep(0,4)), 
                         lb90=c(rep(NA, 4), rep(NA, 4), rep(0,4)),  
                         up90=c(rep(NA, 4), rep(NA, 4), rep(0,4)))) %>% 
    mutate(subsample=subsample) %>% 
    mutate(term=fct_rev(fct_relevel(term, 
                                    "Header:", "    Liberal Media", "    Conservative Media", " ", 
                                    "Text:", "    Anti Government", "    Neutral Text", "  ", 
                                    "Picture:", "    Militarization", "    Kids", "    Neutral Image", "   ", 
                                    "Foot:", "    High Support", "    Low Support", "    ")))
  
}




# Fix Levels --------------------------------------------------------------


d_conjoint <- d_conjoint %>% 
  mutate(text=fct_relevel(text, "Neutral"), 
         picture=fct_relevel(picture, "Neutral"), 
         foot=fct_relevel(foot, "Low_Support"))

table(d_conjoint$foot)

# Partisanship ------------------------------------------------------------

d_conjoint_morena <- d_conjoint %>% 
  filter(vote_morena=="On")

d_conjoint_pan<- d_conjoint %>% 
  filter(vote_pan=="On")

d_conjoint_pri <- d_conjoint %>% 
  filter(vote_pri=="On")

d_conjoint_ind <- d_conjoint %>% 
  filter(vote_pri=="Off"&vote_pan=="Off"&vote_morena=="Off")

table(d_conjoint_ind$vote_morena)

# Models
list_data <- list(d_conjoint, d_conjoint_morena, d_conjoint_pan, d_conjoint_pri , d_conjoint_ind)
list_subsamples <- list("All", "MORENA Voters", "PAN Voters", "PRI Voters", "Independents Voters")


# Share

res <- map2_dfr(list_data, list_subsamples, 
                ~ lm(rec_conjoint_share ~ header  + text + picture + foot,
                     data=.x) %>% 
                  tidy_model(., subsample = .y)) %>% 
          mutate(subsample=fct_relevel(subsample, "All", "MORENA Voters", 
                                       "PAN Voters", "PRI Voters", "Independents Voters"))

# Conjoint Estimates
ggplot(res, aes(y=estimate, x=term, 
                ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Mexico: Conjoint Estimates: Probability of Sharing a Tweet",
       subtitle="Conditional Effects by Vote") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  facet_grid(~subsample, scales = "free") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))

# not share

res_not_share <- map2_dfr(list_data, list_subsamples, 
                          ~ lm(rec_conjoint_share ~ header  + text + picture + foot,
                               data=.x) %>% 
                            tidy_model(., subsample = .y))


ggplot(res_not_share, aes(y=estimate, x=term, 
                          ymin=up, ymax=lb, color=subsample)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Conjoint Estimates: Probability of Not Sharing a Tweet", 
       subtitle="Conditional Effects by Likely to Vote") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") +
  facet_grid(~subsample, scales = "free") +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))


# Victimiation ------------------------------------------------------------

library(janitor)
d_conjoint %>% 
  tabyl(crime_victimization, police_victimization)

# list levels

list_vict <- list("Victim"= "Si", 
                  "Not Victim" = "No")

# Recode
d_vict <- d_conjoint %>% 
  mutate_at(vars(crime_victimization, police_victimization), ~
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x)) %>%        
  mutate_at(vars(crime_victimization, police_victimization), ~
              fct_recode(.x,!!!list_vict))

d_vict %>% 
  tabyl(crime_victimization, police_victimization)


res_vict <- d_vict %>% 
  group_by(crime_victimization) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, crime_victimization, 
                    ~ lm(rec_conjoint_share ~ header  + text + foot + picture,
                         data=.x) %>% 
                      tidy_model(., subsample = .y))) %>% 
  unnest(model)

# Conjoint Estimates

ggplot(res_vict, aes(y=estimate, x=term, 
                     ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Conjoint Estimates: Probability of Sharing a Tweet",
       subtitle="Conditional Effects by Crime Victimization") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  facet_wrap(~subsample,ncol=1) +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))



# Police Victimization ----------------------------------------------------

res_vict <- d_vict %>% 
  group_by(police_victimization) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, police_victimization, 
                    ~ lm(rec_conjoint_share ~ header  + text + foot + picture,
                         data=.x) %>% 
                      tidy_model(., subsample = .y))) %>% 
  unnest(model)


# Conjoint Estimates

ggplot(res_vict, aes(y=estimate, x=term, 
                     ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Conjoint Estimates: Probability of Sharing a Tweet",
       subtitle="Conditional Effects by Crime Victimization") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  facet_wrap(~subsample,ncol=1) +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))

# Fear -----------------------------------------------
table(d_conjoint$fear_1)
list_fear <- list("Very Safe"= "Muy Seguro", 
                  "Not Safe"="Nada Seguro", 
                  "Somewhat Safe"="Seguro", 
                  "Somewhat Unsafe"="Poco Seguro")


# Recode
d_fear <- d_conjoint %>%  
  mutate_at(vars(fear_1, fear_2, 
                 fear_3, fear_4), ~ na_if(.x, "-999")) %>%
  mutate_at(vars(fear_1, fear_2, 
                 fear_3, fear_4), ~ fct_relevel(fct_recode(.x, !!!list_fear), 
                                                "Very Safe", "Somewhat Safe", "Somewhat Unsafe"))

res_fear <-  d_fear %>% 
  group_by(fear_1) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, fear_1,
                    ~ lm(rec_conjoint_share ~ header  + text + foot + picture,
                         data=.x) %>% 
                      tidy_model(., subsample = .y))) %>% 
  unnest(model)


# Conjoint Estimates

ggplot(res_fear, aes(y=estimate, x=term, 
                     ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Conjoint Estimates: Probability of Sharing a Tweet",
       subtitle="Conditional Effects by Fear of Walking on a Dark Street") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  facet_wrap(~subsample,ncol=2) +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))



# Fear 

res_fear <-  d_fear %>% 
  group_by(fear_4) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, fear_4,
                    ~ lm(rec_conjoint_share ~ header  + text + foot + picture,
                         data=.x) %>% 
                      tidy_model(., subsample = .y))) %>% 
  unnest(model)


# Conjoint Estimates

ggplot(res_fear, aes(y=estimate, x=term, 
                     ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Conjoint Estimates: Probability of Sharing a Tweet",
       subtitle="Conditional Effects by Fear upon Crossing with a Police Patrol") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  facet_wrap(~subsample,ncol=2) +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))



# Law and Order -----------------------------------------------------------

# Recode
d_lo <- d_conjoint %>%  
  mutate_at(vars(lo_1, lo_2, 
                 lo_3, lo_4, 
                 lo_5), ~ na_if(.x, "-999")) 

res_fear <-  d_lo %>% 
  group_by(lo_4) %>% 
  nest() %>% 
  drop_na() %>%
  mutate(model=map2(data, lo_4,
                    ~ lm(rec_conjoint_share ~ header  + text + foot + picture,
                         data=.x) %>% 
                      tidy_model(., subsample = .y))) %>% 
  unnest(model)


# Conjoint Estimates

ggplot(res_fear, aes(y=estimate, x=term, 
                     ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Point Estimates", 
       title = "Conjoint Estimates: Probability of Sharing a Tweet",
       subtitle="Conditional Effects by Aggreement to 'Good thug is dead thug'") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  facet_wrap(~subsample,ncol=2) +
  coord_flip() + 
  theme(axis.text.y=element_text(size=16,hjust=0))


