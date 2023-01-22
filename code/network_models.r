##############################################################################
# File-Name: network_model.r
# Date: January 21 2023
# author: Tiago Ventura 
# Purpose: this code produces the network measure of contextual victimization. 
#The output of this code is used in the `analysis_main_paper.r` code
# Data in: "data/raw_survey_data.rdata"
# Data out: "data/processed_survey_data.rds"
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
