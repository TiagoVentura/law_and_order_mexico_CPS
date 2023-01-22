##############################################################################
# File-Name: clean_survey_data.r
# Date: January 21 2023
# author: Tiago Ventura 
# Purpose: this script cleans the survey data and prepares data for the conjoint analysis
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
#source(here("code", "utils.R"))
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")


# open data -------------------------------------------------------
load("./data/raw_survey_data.Rdata")

# Cleaning the Conjoints --------------------------------------------------

## Putting together the two taks. 

# outcomes of the two conjoint tasks
outcomes <- d %>% 
  select(responseid,conjoint1_outcome, conjoint2_outcome)

# convert from wide to long
outcomes <- outcomes  %>% 
  pivot_longer(cols = -responseid, 
               names_to = "var", 
               values_to = "outcome") %>% 
  mutate(task= ifelse(var=="conjoint1_outcome", "choice_1", "choice_2"), 
         candidate= ifelse(outcome=="Candidato A", "cd_1", "cd_2"), 
         missing_id= ifelse(is.na(outcome)|outcome=="-999", 1, 0), 
         candidate=ifelse(missing_id==1, "cd_1", candidate)) 


# Conjoint features

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

# convert to long
features_wider <- features_long %>% select(-var) %>%
  pivot_wider(
    names_from = features,  
    values_from= value) 


### Put together with the outcomes

conjoint_data_to_remove <- left_join(features_wider,outcomes, by=c("responseid", "task"))

# remove missing
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

# Combine -----------------------------------------------------------------

# merge conjoint long with the raw survey data
conjoint_data <- left_join(d, conjoint_data)

# relevel the conjoint data to stick with the baselines
conjoint_data <- conjoint_data %>% 
  mutate_at(vars(contains("feat_")), as_factor) %>%
  mutate(feat_political_party=fct_relevel(feat_political_party, "Independiente"), 
         feat_gender=fct_relevel(feat_gender, "Mujer"), 
         feat_security_proposal= fct_relevel(feat_security_proposal, 
                                             "Creación de un centro de asesoría y atención a víctimas"),
         feat_occupation= fct_relevel(feat_occupation, "Empleado público"))

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
# define order here
conjoint_data <- conjoint_data %>% 
  mutate(feat_gender=fct_recode(feat_gender, !!!levels_gender), 
         feat_political_party=fct_recode(feat_political_party, !!!levels_party), 
         feat_occupation=fct_recode(feat_occupation, !!!levels_occup), 
         feat_security_proposal=fct_recode(feat_security_proposal,
                                           !!!levels_sec)) 

# Save data as processed conjoint data
#write_rds(conjoint_data, here("data", "processed_survey_data.rds"))

# session
sessionInfo()

# R version 4.0.3 (2020-10-10)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 20.04.2 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
# [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
# [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
# [10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] broom_0.7.7          extrafont_0.17       patchwork_1.0.1      rebus_0.1-3         
# [5] conflicted_1.0.4     scales_1.1.1         lubridate_1.8.0      summarytools_0.9.6  
# [9] forcats_0.5.0        stringr_1.4.0        dplyr_1.0.8          purrr_0.3.4         
# [13] readr_2.1.2          tidyr_1.2.0          tibble_3.1.6         ggplot2_3.3.5       
# [17] tidyverse_1.3.0.9000 here_1.0.1          
# 
# loaded via a namespace (and not attached):
#   [1] httr_1.4.2            jsonlite_1.8.0        modelr_0.1.8          assertthat_0.2.1     
# [5] pander_0.6.3          cellranger_1.1.0      yaml_2.3.5            Rttf2pt1_1.3.8       
# [9] pillar_1.7.0          backports_1.4.1       glue_1.6.2            extrafontdb_1.0      
# [13] digest_0.6.29         pryr_0.1.4            checkmate_2.0.0       rvest_1.0.2          
# [17] colorspace_2.0-3      htmltools_0.5.2       plyr_1.8.6            pkgconfig_2.0.3      
# [21] rebus.unicode_0.0-2   haven_2.3.1           magick_2.4.0          rebus.numbers_0.0-1  
# [25] tzdb_0.2.0            generics_0.1.2        ellipsis_0.3.2        cachem_1.0.5         
# [29] withr_2.5.0           cli_3.2.0             magrittr_2.0.2        crayon_1.5.0         
# [33] readxl_1.3.1          memoise_2.0.0         evaluate_0.15         fs_1.5.0             
# [37] fansi_1.0.2           rebus.base_0.0-3      xml2_1.3.3            rapportools_1.0      
# [41] tools_4.0.3           hms_1.1.1             lifecycle_1.0.1       matrixStats_0.57.0   
# [45] munsell_0.5.0         reprex_2.0.1          compiler_4.0.3        rlang_1.0.2          
# [49] grid_4.0.3            rstudioapi_0.13       tcltk_4.0.3           base64enc_0.1-3      
# [53] rmarkdown_2.10        gtable_0.3.0          codetools_0.2-16      DBI_1.1.2            
# [57] R6_2.5.1              knitr_1.37            fastmap_1.1.0         utf8_1.2.2           
# [61] rprojroot_2.0.2       rebus.datetimes_0.0-1 stringi_1.7.6         Rcpp_1.0.8           
# [65] vctrs_0.3.8           dbplyr_2.1.1          tidyselect_1.1.2      xfun_0.30   