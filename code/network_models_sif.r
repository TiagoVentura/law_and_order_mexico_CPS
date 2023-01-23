##############################################################################
# File-Name: network_model_sif.r
# Date: January 21 2023
# author: Tiago Ventura 
# Purpose: this code produces the results for appendix A 
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
