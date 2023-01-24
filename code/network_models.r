##############################################################################
# File-Name: network_model.r
# Date: January 21 2023
# author: Tiago Ventura 
# Purpose: this code produces the network measure of contextual victimization. 
#The output of this code is used in the `analysis_main_paper.r` code
# Data in: "data/raw_survey_data.rdata"
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

# load the data -------------------------------------------------------

load("./data/raw_survey_data.Rdata")

#get all the variables with the network information -----------------------------------------------------------------


network <- d %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999")))) %>%
  select(responseid, starts_with("network"), -contains("click"), - contains("submit"))


# long format

network_long <- network %>%
  pivot_longer(cols=-(responseid), 
               names_to="group", 
               values_to="values") 

# first set of questions
howmany <- stack(network[,c(2:9)])

# Modeling I --------------------------------------------------------------
n.row= nrow(network)
n.cat= 8

# values
howmany$values<-as.integer(howmany[,1])

#id
id <- rep(unique(network$responseid),n.cat)

# category
category <- rep(1:n.cat,each=n.row)

# N
N <- length(howmany$values)
C <- max(category)
G <- length(unique(id))

##
## Personal Network
##
library(lme4)

# modeling
net.lmer <-glmer.nb(howmany$values ~ 1 + (1|category) + (1|id),  nAGQ=0)

# parametera
a.personal<- coef(net.lmer)

# dataframe
net.personal.lmer <- data_frame(values=a.personal[[1]][,1], 
                                responseid=row.names(a.personal[[1]]))


# values
net.personal.lmer$values_adj <- exp(net.personal.lmer$values)/0.008

##
## Group Network
##


# Filter values with offsets
network_filter <- network %>%
  left_join(net.personal.lmer) %>%
  filter(!is.na(values))    

n.row= nrow(network_filter)
n.cat= 8

# second stage
howmany<- stack(network_filter[,10:17])
howmany$values<-as.integer(howmany[,1])

# offset
offset2=rep(net.personal.lmer$values_adj,n.cat)

# vars
id <- rep(unique(network_filter$responseid),n.cat)
category <- rep(1:n.cat,each=n.row)
N <- length(howmany$values)
C <- max(category)
G <- length(unique(id))


# model with offset
net.lmer2<-glmer.nb(howmany$values ~ 1 + offset(log(offset2)) + (1|category) + (1|id), nAGQ=0)

# individual size parameter
a.final<- ranef(net.lmer2)
net.cat.lmer<-a.final[[2]][,1]

# create repo
net.greg.lmer<-data_frame(values=a.final[[1]][,1], 
                          responseid=row.names(a.final[[1]]))

# group size
group <- exp(coef(net.lmer2)[[2]][1])

str_remove(colnames(network_filter)[10:17],"network_")

colnames(network_filter)[10:17]

rownames(group)<- str_remove(colnames(network_filter)[10:17],"network_")


##
## Full Network
##

ab <- vector("list",n.cat)
residuals <- vector("list",n.cat)

# prediction
for (i in 1:n.cat){
  ab[[i]]<- data_frame(yhat=exp(net.cat.lmer[i]+ net.greg.lmer$values), 
                       responseid=net.greg.lmer$responseid,
                       group=colnames(network_filter)[10:17][[i]])
} 

# cpombine
ab2<- bind_rows(ab)
ab2 <- left_join(ab2, network_long) %>%
  mutate(res=sqrt(values)-sqrt(yhat))

# saving the data
crime <- ab2 %>% filter(group %in% c("network_victim_crime", "network_police_violence")) 

#save(crime, file=here("data", "network_results.Rdata"))



# Appendix A --------------------------------------------------------------

# Here, I need to re-estimate the model to get the figures with all the k categories

network_filter <- network %>%
  left_join(net.personal.lmer) %>%
  filter(!is.na(values))    

# number of rows and columanes
n.row= nrow(network_filter)
n.cat= 16

# separate the dc
howmany<- stack(network_filter[,2:17])
howmany$values<-as.integer(howmany[,1])
net.personal.lmer$values_adj <- exp(net.personal.lmer$values)/0.005

# get the offset
offset2=rep(net.personal.lmer$values_adj,n.cat)
id <- rep(unique(network_filter$responseid),n.cat)
category <- rep(1:n.cat,each=n.row)

# containers
N <- length(howmany$values)
C <- max(category)
G <- length(unique(id))

# model
net.lmer2<-glmer.nb(howmany$values ~ 1 + offset(log(offset2)) + (1|category) + (1|id), nAGQ=0)

# parameter alpha
a.final<- ranef(net.lmer2)
net.cat.lmer<-a.final[[2]][,1]

# container
net.greg.lmer<-data_frame(values=a.final[[1]][,1], 
                          responseid=row.names(a.final[[1]]))

# groupsize
group <- exp(coef(net.lmer2)[[2]][1])

# names
str_remove(colnames(network_filter)[2:17],"network_")
colnames(network_filter)[2:17]
rownames(group)<- str_remove(colnames(network_filter)[2:17],"network_")

##
## Full Network
##

ab <- vector("list",n.cat)
residuals <- vector("list",n.cat)

# get predicted values
for (i in 1:n.cat){
  ab[[i]]<- data_frame(yhat=exp(net.cat.lmer[i]+ net.greg.lmer$values), 
                       responseid=net.greg.lmer$responseid,
                       group=colnames(network_filter)[2:17][[i]])
} 

# bind rows
ab2<- bind_rows(ab)

# calculate residuals
ab2 <- left_join(ab2, network_long) %>%
  mutate(res=sqrt(values)-sqrt(yhat))

# matrix
resid.mat <- matrix(ab2$res,length(unique(ab2$responseid)),length(unique(ab2$group)))
colnames(resid.mat) <- colnames(network_filter)[2:17]
network.matrix<-cor(resid.mat,use="complete.obs")
names <- str_remove(colnames(network.matrix), "network_") %>%
  str_replace(., "_", " ") %>%
  str_to_title(.)

# names
names <- c("Silvia", "Antonio", "Police Officer", "Physician", "Had a Kid", "Died", "Got Married", 
           "Died from Covid", "Public Empoloyee", "Does Social Work", "Partisan Activist", "Was Candidate", 
           "Crime Victim", "Police Violence", "Is in Prision", "Lost Job (Covid)")
colnames(network.matrix) <- names
rownames(network.matrix) <- names

##
## Correlation Plots
##

#Agnes Cluster
library(cluster)
dissimilarity <- 1 - network.matrix
distance <- as.dist(dissimilarity)

# ggplot correlation
library(ggcorrplot)
ggcorrplot(
  network.matrix,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_minimal(),
  colors = c("#6D9EC1", "white", "tomato2")) 

ggsave(filename=here("output", "sif_correlation_matrix.png") ,
       width = 12, height = 8, units = "in", pointsize = 12, bg = "white")

# dendogram
library(ggdendro)
hc <- hclust(distance, "ave")
ggdendrogram(hc, xlab="")
png(filename=here("output", "sif_fig1_sif.png"))
plot(hc, xlab="")
dev.off()


# separate crime and police
crime <- ab2 %>% filter(group %in% c("network_victim_crime", "network_police_violence")) 

# convert to wide
crime_wide <- crime %>% 
  pivot_wider(names_from=group, 
              values_from = c(yhat, res, values))


# Table 2 SIF -------------------------------------------------------------

# load data
load("./data/raw_survey_data.Rdata")

# clean
d <- left_join(d, crime_wide) %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999"))))


# Clean Covariates
d <- d %>%
  dplyr::mutate(across(starts_with("network"), ~  
                         as.numeric(na_if(., "-999")))) %>%
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4, contains("po_"), contains("issue_")), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x)) %>%
  mutate_at(vars(fear_1, fear_2, fear_3, fear_4), ~ 
              fct_relevel(.x, "Muy Seguro", "Seguro", "Poco Seguro")) %>%
  mutate_at(vars(lo_1, lo_2, lo_3, lo_4, lo_5), ~ 
              ifelse(.x %in% c("-999", "No lo se"), 
                     NA, .x))  %>% 
  mutate_at(vars(crime_victimization, police_victimization, income_assistance,), ~
              ifelse(.x %in% c("-999", "No lo se", "No se"), 
                     NA, .x)) %>%
  mutate(trust_police=as.numeric(na_if(trust_police,"-999")))  %>%
  mutate(income_quantile=ntile(income, 4), 
         income_quantile_labels=ifelse(income_quantile==4, "Top-Quartile", 
                                       ifelse(income_quantile==1, "Bottom-Quartile", 
                                              "Middle"))) %>%
  rowwise() %>%
  mutate(fear_av=mean(c(fear_1, fear_2, fear_3), na.rm = TRUE)) %>%
  ungroup()



# invert preferences for law and order
list_lo_invert <- list("0"="5",
                       "1"="4",
                       "2"="3", 
                       "3"="1",
                       "4"="0")

list_lo_correct <- list("0"="0",
                        "1"="1",
                        "2"="3", 
                        "3"="4",
                        "4"="5")

d <- d %>%  
  mutate_at(vars(lo_1, lo_2, 
                 lo_3, lo_4, 
                 lo_5), ~ na_if(.x, "-999")) %>%
  mutate_at(vars(lo_1, lo_2, lo_3), ~ fct_recode(.x, !!!list_lo_correct)) %>% 
  mutate_at(vars(lo_4, lo_5), ~ fct_recode(.x, !!!list_lo_invert)) %>% 
  mutate_at(vars(lo_1, lo_2, lo_3, lo_4, lo_5), ~ as.numeric(as.character(.x))) %>%
  rowwise() %>%
  mutate(lo_av=mean(c(lo_1, lo_2, lo_3, lo_4, lo_5), na.rm = TRUE))


# Crime victimization
model_crime <- tribble(
  ~model_name, ~formula, ~data, 
  "Crime Victimization", as.numeric(as.factor(crime_victimization)) ~ 
    income_quantile_labels +
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    police_victimization +
    as.numeric(po_criminal_rights) +fear_av +
    trust_police +
    as.numeric(issue_security), d, 
  "Network Raw", as.numeric(network_victim_crime) ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization  + as.numeric(po_criminal_rights) +
    fear_av + trust_police +
    as.numeric(issue_security), d, 
  "Network Poisson Model", res_network_victim_crime ~ 
    income_quantile_labels +
    work  +
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization + as.numeric(po_criminal_rights) +
    fear_av + trust_police +
    as.numeric(issue_security), d)


library(broom)

list_terms <- list("Income (Top Quartile)"="income_quantile_labelsTop-Quartile", 
                   "Income (Middle)"="income_quantile_labelsMiddle",
                   "Employed"="workEmployed", 
                   "Female"="genderFemale", 
                   "Crime Victim"="crime_victimizationSi",             
                   "Police Violence"="police_victimizationSi", 
                   "Fear of Crime"="fear_av",
                   "Punitive Preference"="as.numeric(po_criminal_rights)", 
                   "Trust Police"="trust_police", 
                   "Age"="as.numeric(age)", 
                   "Education"="as.numeric(education)", 
                   "Security (Policy Priority)"="as.numeric(issue_security)")

model_crime <- model_crime %>%
  rowwise() %>%
  mutate(model=list(lm(formula, data)), 
         parameters=list(tidy(model))) %>% 
  unnest(parameters) %>%
  mutate(lb=estimate - 1.96*std.error, 
         up= estimate + 1.96*std.error, 
         lb90=estimate - 1.64*std.error, 
         up90= estimate + 1.64*std.error) %>% 
  filter(!(term %in% c("(Intercept)"))) %>%
  mutate(term=fct_inorder(fct_recode(term,!!!list_terms)))

# as a graph
ggplot(model_crime, aes(y=estimate, x=fct_rev(term), 
                        ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Estimates", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~model_name, scales="free") +
  theme(axis.text.y=element_text(size=16))

# model police victimization
model_police <- tribble(
  ~model_name, ~formula, ~data, 
  "Police Victimization", as.numeric(as.factor(police_victimization)) ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization +
    as.numeric(po_criminal_rights) +
    fear_av  + trust_police +
    as.numeric(issue_security), d, 
  "Network Raw", as.numeric(network_police_violence) ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization +  as.numeric(po_criminal_rights) +
    fear_av  + trust_police +
    as.numeric(issue_security), d, 
  "Network Poisson Model", res_network_police_violence ~ 
    income_quantile_labels +  
    work + 
    as.numeric(age)+
    as.numeric(education) +
    gender +
    crime_victimization + 
    police_victimization + as.numeric(po_criminal_rights) +
    fear_av + trust_police +
    as.numeric(issue_security), d) 


# clean
model_police <- model_police %>%
  rowwise() %>%
  mutate(model=list(lm(formula, data)), 
         parameters=list(tidy(model))) %>% 
  unnest(parameters) %>%
  mutate(lb=estimate - 1.96*std.error, 
         up= estimate + 1.96*std.error, 
         lb90=estimate - 1.64*std.error, 
         up90= estimate + 1.64*std.error) %>% 
  filter(!(term %in% c("(Intercept)"))) %>%
  mutate(term=fct_inorder(fct_recode(term,!!!list_terms)))

# as a graph
ggplot(model_police, aes(y=estimate, x=fct_rev(term), 
                         ymin=up, ymax=lb)) +
  geom_pointrange(size=1, fill="black", shape=21, color="grey5",
                  position=position_dodge(width = .6), alpha=.8) +
  geom_pointrange(aes(ymin=lb90, y=estimate, ymax=up90), shape=21, 
                  size=1.5, color="grey5") +
  labs(x="", y="Estimates", 
       title = "") +
  geom_hline(yintercept = 0, linetype="dashed", color="darkred") + 
  scale_color_brewer(palette="Set1") +
  coord_flip() + 
  facet_grid(~model_name, scales="free") +
  theme(axis.text.y=element_text(size=16,hjust=1))


## latex table

# bind the models
models <- bind_rows(model_crime, model_police) %>%
  select(model_name, model) %>%
  distinct()

# call libraries

# Labels
new_names = c("(Intercept)" = "Intercept", 
              "income_quantile_labelsMiddle" =  "Income (Middle)",             
              "income_quantile_labelsTop-Quartile" = "Income (Top Quartile)",
              "workEmployed"   ="Employed",          
              "as.numeric(age)"  = "Age",
              "as.numeric(education)"= "Education",                                
              "genderFemale"     =   "Female",
              "crime_victimizationSi" = " Crime Victim",
              "police_victimizationSi"     = "Police Violence Victim",
              "as.numeric(po_criminal_rights)"  =  "Punitive Preferences",
              "fear_av"    =  "Fear of Crime",  
              "trust_police" = "Trust in the Police", 
              "as.numeric(issue_security)" = "Security Top Priority")
library(stargazer)
stargazer(models$model[[3]], models$model[[1]], models$model[[6]],models$model[[4]], 
          intercept.bottom = FALSE, 
          dep.var.labels.include = FALSE,
          column.labels = c("\\multirow{2}{*}{\\parbox{5cm}{Crime Victimization (Network Residuals)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{crime Victimization (Survey Questions)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{Police Violence (Network Residuals)}}", 
                            "\\multirow{2}{*}{\\parbox{5cm}{Police Violence (Survey Questions)}}"),
          omit.stat = c("rsq", "f", "ser"), 
          covariate.labels =new_names, 
          model.numbers = FALSE, 
          title="Regression Estimates: Correlates of Contextual and Individual Victimization", 
          label="reg_net", 
          out=here("output", "sif_table_1_sif.tex"))


